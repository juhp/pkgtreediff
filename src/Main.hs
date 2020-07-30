{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
    (<|>),
#endif
#if !MIN_VERSION_base(4,8,0)
    (<$>), (<*>)
#endif
  )
import Control.Concurrent.Async (concurrently)
import Control.Monad
import Control.Monad.Extra (concatMapM)
import Data.List
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (managerResponseTimeout, newManager,
                            responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Directory
import SimpleCmdArgs
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, match)
#if !MIN_VERSION_simple_cmd(0,2,0)
-- for warning
import System.IO (hPutStrLn, stderr)
#endif

import SimpleCmd (cmd, error',
#if MIN_VERSION_simple_cmd(0,2,0)
                  warning
#endif
                 )

import qualified Fedora.Koji as Koji

import Distribution.RPM.PackageTreeDiff
import Paths_pkgtreediff (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees or instances" $
    compareDirs <$> recursiveOpt <*> ignoreVR <*> ignoreArch <*> modeOpt  <*> optional patternOpt <*> timeoutOpt <*> sourceArg <*> sourceArg
  where
    sourceArg :: Parser String
    sourceArg = strArg "URL|TAG|DIR|FILE|CMD1"

    modeOpt :: Parser Mode
    modeOpt =
      flagWith' Added 'N' "new" "Show only added packages" <|>
      flagWith' Deleted 'D' "deleted" "Show only removed packages" <|>
      flagWith' Updated 'U' "updated" "Show only updated packages" <|>
      flagWith' ShowSummary 's' "show-summary" ("Show summary of changes (default when >" <> show summaryThreshold <> " changes)") <|>
      flagWith AutoSummary NoSummary 'S' "no-summary" "Do not display summary"

    ignoreArch :: Parser Bool
    ignoreArch = switchWith 'A' "ignore-arch" "Ignore arch differences"

    ignoreVR :: Parser Ignore
    ignoreVR =
      flagWith' IgnoreRelease 'R' "ignore-release" "Only show version changes (ignore release)" <|>
      flagWith IgnoreNone IgnoreVersion 'V' "ignore-version" "Only show package changes (ignore version-release)"

    recursiveOpt :: Parser Bool
    recursiveOpt = switchWith 'r' "recursive" "Recursive down into subdirectories"

    patternOpt :: Parser String
    patternOpt = strOptionWith 'p' "pattern" "PKGPATTERN" "Limit packages to glob matches"

    timeoutOpt :: Parser Int
    timeoutOpt = optionalWith auto 't' "timeout" "SECONDS" "Maximum seconds to wait for http response before timing out (default 30)" 30

-- | The threshold for the number of differences for compareDirs to auto-output a summary
summaryThreshold :: Int
summaryThreshold = 20

data SourceType = URL | Tag | Dir | File | Cmd
  deriving Eq

-- >>> kojiUrlTag "koji://tag@fedora"
-- Just ("tag", "https://koji.fedoraproject.org/kojihub")
kojiUrlTag :: String -> Maybe (String, String)
kojiUrlTag s = if not (isKojiScheme s) then Nothing else case elemIndex '@' s of
    Just pos -> Just $ (drop (length kojiScheme) $ take pos s, hubUrl $ drop (pos+1) s)
    Nothing -> Nothing
  where
    kojiScheme = "koji://"
    isKojiScheme loc = kojiScheme `isPrefixOf` loc
    hubUrl "fedora" = Koji.fedoraKojiHub
    hubUrl "centos" = Koji.centosKojiHub
    hubUrl loc = loc

sourceType :: String -> IO SourceType
sourceType s
  | isHttp s  = return URL
  | isKoji s  = return Tag
  | otherwise = do
      dir <- doesDirectoryExist s
      if dir then return Dir
        else if ' ' `elem` s then return Cmd
             else return File
  where
    isKoji :: String -> Bool
    isKoji loc = isJust (kojiUrlTag loc)
    isHttp :: String -> Bool
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

-- | Frontend for the pkgtreediff tool
compareDirs :: Bool -> Ignore -> Bool -> Mode -> Maybe String -> Int -> String -> String -> IO ()
compareDirs recursive ignore igArch mode mpattern timeout tree1 tree2 = do
  (ps1,ps2) <- getTrees tree1 tree2
  let diff = diffPkgs ignore ps1 ps2
  mapM_ T.putStrLn . mapMaybe (showPkgDiff mode) $ diff
  when (mode /= NoSummary && isDefault mode) $
    when (mode == ShowSummary || length diff > summaryThreshold) $ do
    putStrLn ""
    putStrLn "Summary"
    let diffsum = summary diff
    putStrLn $ "Updated: " <> show (updateSum diffsum)
    putStrLn $ "Added: " <> show (newSum diffsum)
    putStrLn $ "Deleted: " <> show (delSum diffsum)
    putStrLn $ "Arch changed: " <> show (archSum diffsum)
    putStrLn $ "Total packages: " <> show (length ps1) <> " -> " <> show (length ps2)
  where
    getTrees :: String -> String -> IO ([RpmPackage],[RpmPackage])
    getTrees t1 t2 = do
      when (t1 == t2) $ warning "Comparing the same tree!"
      src1 <- sourceType t1
      src2 <- sourceType t2
      mmgr <- if src1 == URL || src2 == URL
        then do
        let ms = responseTimeoutMicro $ timeout * 1000000
        Just <$> newManager tlsManagerSettings {managerResponseTimeout = ms}
        else return Nothing
      let act1 = readPackages src1 mmgr t1
          act2 = readPackages src2 mmgr t2
      if (src1,src2) == (Cmd,Cmd)
        then do
        ps1 <- act1
        ps2 <- act2
        return (ps1,ps2)
        else concurrently act1 act2

    readPackages source mmgr loc = do
      fs <- case source of
              URL -> httpPackages True (fromJust mmgr) loc
              Tag -> kojiPackages (fromJust (kojiUrlTag loc))
              Dir -> dirPackages True loc
              File -> filePackages loc
              Cmd -> cmdPackages $ words loc
      let ps = map ((if igArch then dropRpmArch else id) . readRpmPkg) $ filter (maybe (const True) (match . compile) mpattern . T.unpack) fs
      return $ sort (nub ps)

    httpPackages recurse mgr url = do
      exists <- httpExists mgr url
      fs <- if exists
            then filter (\ f -> "/" `T.isSuffixOf` f || ".rpm" `T.isSuffixOf` f) <$> httpDirectory mgr url
            else error' $ "Could not get " <> url
      if (recurse || recursive) && all isDir fs then concatMapM (httpPackages False mgr) (map ((url </>) . T.unpack) fs) else return $ filter (not . isDir) fs

    dirPackages recurse dir = do
      -- can replace with listDirectory after dropping ghc7
      -- should really filter out ".rpm" though not common
      fs <- sort . filter (".rpm" `isSuffixOf`) <$> getDirectoryContents dir
      alldirs <- mapM doesDirectoryExist fs
      if (recurse || recursive) && and alldirs then concatMapM (dirPackages False) (map (dir </>) fs) else return $ filter (not . isDir) $ map T.pack fs

    isDir = ("/" `T.isSuffixOf`)

    filePackages file =
      filter (not . T.isPrefixOf (T.pack "gpg-pubkey-")) . T.words <$> T.readFile file

    cmdPackages [] = error' "No command prefix given"
    cmdPackages (c:args) =
      -- use words since container seems to append '\r'
      filter (not . T.isPrefixOf (T.pack "gpg-pubkey-")) . T.words . T.pack <$> cmd c args

    kojiPackages (tag, kojiUrl) = map (T.pack . Koji.kbNvr) <$> Koji.kojiListTaggedBuilds kojiUrl True tag

isDefault :: Mode -> Bool
isDefault m = m `elem` [AutoSummary, NoSummary, ShowSummary]

showPkgDiff :: Mode -> RpmPackageDiff -> Maybe Text
showPkgDiff Added (PkgAdd p) = Just $ showRpmPkg p
showPkgDiff Deleted (PkgDel p) = Just $ showRpmPkg p
showPkgDiff Updated (PkgUpdate p1 p2) = Just $ showPkgUpdate p1 p2
showPkgDiff Updated (PkgArch p1 p2) = Just $ showArchChange p1 p2
showPkgDiff mode (PkgAdd p) | isDefault mode = Just $ "+ " <> showRpmPkg p
showPkgDiff mode (PkgDel p) | isDefault mode  = Just $ "- " <> showRpmPkg p
showPkgDiff mode (PkgUpdate p1 p2) | isDefault mode = Just $ showPkgUpdate p1 p2
showPkgDiff mode (PkgArch p1 p2) | isDefault mode = Just $ "! " <> showArchChange p1 p2
showPkgDiff _ _ = Nothing

showPkgUpdate :: RpmPackage -> RpmPackage -> Text
showPkgUpdate p p' =
  rpmPkgIdent p <> ": " <> rpmPkgVerRel p <> " -> " <> rpmPkgVerRel p'

showArchChange :: RpmPackage -> RpmPackage -> Text
showArchChange p p' =
  rpmName p <> ": " <> rpmDetails p <> " -> " <> rpmDetails p'
  where
    rpmDetails :: RpmPackage -> Text
    rpmDetails pkg = rpmPkgVerRel pkg <> appendArch pkg

data DiffSum = DS {updateSum, newSum, delSum, archSum :: Int}

emptyDS :: DiffSum
emptyDS = DS 0 0 0 0

summary :: [RpmPackageDiff] -> DiffSum
summary =
  foldl' countDiff emptyDS
  where
    countDiff :: DiffSum -> RpmPackageDiff -> DiffSum
    countDiff ds pd =
      case pd of
        PkgUpdate {} -> ds {updateSum = updateSum ds + 1}
        PkgAdd _ -> ds {newSum = newSum ds + 1}
        PkgDel _ -> ds {delSum = delSum ds + 1}
        PkgArch {} -> ds {archSum = archSum ds + 1}

#if !MIN_VERSION_simple_cmd(0,2,0)
warning :: String -> IO ()
warning = hPutStrLn stderr
#endif
