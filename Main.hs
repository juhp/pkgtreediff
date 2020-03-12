{-# LANGUAGE CPP #-}

import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
  , (<$>), (<*>)
#endif
  )
import Control.Concurrent.Async (concurrently)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP.Directory
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, match)

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,0))
#else
-- for warning
import System.IO (hPutStrLn, stderr)
#endif

import SimpleCmd (cmd, error',
#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,0))
                  warning
#endif
                 )
import SimpleCmdArgs

import Paths_pkgtreediff (version)

data Mode = AutoSummary | NoSummary | ShowSummary | Added | Deleted | Updated
  deriving Eq

data Ignore = IgnoreNone | IgnoreRelease | IgnoreVersion
  deriving Eq

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees or instances" $
    compareDirs <$> recursiveOpt <*> ignoreVR <*> ignoreArch <*> modeOpt  <*> optional patternOpt <*> strArg "URL|DIR|FILE|CMD1" <*> strArg "URL|DIR|FILE|CMD2"
  where
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

summaryThreshold :: Int
summaryThreshold = 20

data SourceType = URL | Dir | File | Cmd
  deriving Eq

sourceType :: String -> IO SourceType
sourceType s =
  if isHttp s then return URL
  else do
    dir <- doesDirectoryExist s
    if dir then return Dir
      else if ' ' `elem` s then return Cmd
           else return File
  where
    isHttp :: String -> Bool
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

compareDirs :: Bool -> Ignore -> Bool -> Mode -> Maybe String -> String -> String -> IO ()
compareDirs recursive ignore igArch mode mpattern tree1 tree2 = do
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
    getTrees :: String -> String -> IO ([Package],[Package])
    getTrees t1 t2 = do
      when (t1 == t2) $ warning "Comparing the same tree!"
      src1 <- sourceType t1
      src2 <- sourceType t2
      mmgr <- if src1 == URL || src2 == URL then Just <$> httpManager else return Nothing
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
              Dir -> dirPackages True loc
              File -> filePackages loc
              Cmd -> cmdPackages $ words loc
      let ps = map ((if igArch then binToPkg else id) . readPkg) $ filter (maybe (const True) (match . compile) mpattern . T.unpack) fs
      return $ sort (nub ps)

    binToPkg :: Package -> Package
    binToPkg (Pkg n vr _) = Pkg n vr Nothing

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
    cmdPackages (c:cs) = do
      let args = cs ++ ["rpm", "-qa"]
      putStr $ unwords (c:args) ++ "..."
      -- use words since container seems to append '\r'
      res <- filter (not . T.isPrefixOf (T.pack "gpg-pubkey-")) . T.words . T.pack <$> cmd c args
      putStrLn " done"
      return res

type Name = Text
type Arch = Text

data VersionRelease = VerRel Text Text
  deriving Eq

instance Ord VersionRelease where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

data VerChunk = TxtChunk Text | IntChunk Int
  deriving (Eq,Ord)

verChunk :: Text -> VerChunk
verChunk t | T.all isDigit t = (IntChunk . read . T.unpack) t
verChunk t = TxtChunk t

rpmVerCompare :: Text -> Text -> Ordering
rpmVerCompare v1 v2 | v1 == v2 = EQ
rpmVerCompare v1 v2 =
  compare (verList v1) (verList v2)
  where
    verList :: Text -> [VerChunk]
    verList = map verChunk . filter (T.all isAlphaNum) . T.groupBy (\ c1 c2 -> generalCategory c1 == generalCategory c2)

-- eqVR True ignore release
eqVR :: Ignore -> VersionRelease -> VersionRelease -> Bool
eqVR IgnoreNone vr vr' = vr == vr'
eqVR IgnoreRelease (VerRel v _) (VerRel v' _) = v == v'
eqVR IgnoreVersion _ _ = True

verRel :: Package -> Text
verRel = txtVerRel . pkgVerRel
  where
    txtVerRel (VerRel v r) = v <> "-" <> r

data Package = Pkg {pkgName :: Name, pkgVerRel :: VersionRelease, pkgMArch :: Maybe Arch}
  deriving (Eq, Ord)

pkgIdent :: Package -> Text
pkgIdent p  = pkgName p <> appendArch p

pkgDetails :: Package -> Text
pkgDetails p = verRel p <> appendArch p

appendArch :: Package -> Text
appendArch p = maybe "" ("." <>) (pkgMArch p)

showPkg :: Package -> Text
showPkg p = pkgIdent p <> "  " <> verRel p

readPkg :: Text -> Package
readPkg t =
  if compnts < 3 then error' $ T.unpack $ "Malformed rpm package name: " <> t
  else Pkg name (VerRel ver rel) (Just arch)
  where
    compnts = length pieces
    (nvr',arch) = T.breakOnEnd "." $ fromMaybe t $ T.stripSuffix ".rpm" t
    pieces = reverse $ T.splitOn "-" $ T.dropEnd 1 nvr'
    (rel:ver:emaN) = pieces
    name = T.intercalate "-" $ reverse emaN

data PackageDiff = PkgUpdate Package Package
                 | PkgAdd Package
                 | PkgDel Package
                 | PkgArch Package Package
  deriving Eq

isDefault :: Mode -> Bool
isDefault m = m `elem` [AutoSummary, NoSummary, ShowSummary]

showPkgDiff :: Mode -> PackageDiff -> Maybe Text
showPkgDiff Added (PkgAdd p) = Just $ showPkg p
showPkgDiff Deleted (PkgDel p) = Just $ showPkg p
showPkgDiff Updated (PkgUpdate p1 p2) = Just $ showPkgUpdate p1 p2
showPkgDiff Updated (PkgArch p1 p2) = Just $ showArchChange p1 p2
showPkgDiff mode (PkgAdd p) | isDefault mode = Just $ "+ " <> showPkg p
showPkgDiff mode (PkgDel p) | isDefault mode  = Just $ "- " <> showPkg p
showPkgDiff mode (PkgUpdate p1 p2) | isDefault mode = Just $ showPkgUpdate p1 p2
showPkgDiff mode (PkgArch p1 p2) | isDefault mode = Just $ "! " <> showArchChange p1 p2
showPkgDiff _ _ = Nothing

showPkgUpdate :: Package -> Package -> Text
showPkgUpdate p p' =
  pkgIdent p <> ": " <> verRel p <> " -> " <> verRel p'

showArchChange :: Package -> Package -> Text
showArchChange p p' =
  pkgName p <> ": " <> pkgDetails p <> " -> " <> pkgDetails p'

diffPkgs :: Ignore -> [Package] -> [Package] -> [PackageDiff]
diffPkgs _ [] [] = []
diffPkgs ignore (p:ps) [] = PkgDel p : diffPkgs ignore ps []
diffPkgs ignore [] (p:ps) = PkgAdd p : diffPkgs ignore [] ps
diffPkgs ignore (p1:ps1) (p2:ps2) =
  case compareNames p1 p2 of
    LT -> PkgDel p1 : diffPkgs ignore ps1 (p2:ps2)
    EQ -> let diff = diffPkg ignore p1 p2
              diffs = diffPkgs ignore ps1 ps2
          in if isJust diff then fromJust diff : diffs else diffs
    GT -> PkgAdd p2 : diffPkgs ignore (p1:ps1) ps2

diffPkg :: Ignore -> Package-> Package-> Maybe PackageDiff
diffPkg ignore p1 p2 | pkgIdent p1 == pkgIdent p2 =
                           if eqVR ignore (pkgVerRel p1) (pkgVerRel p2)
                           then Nothing
                           else Just $ PkgUpdate p1 p2
--diffPkg ignore p1 p2 | pkgName p1 == pkgName p2 =
--                            diffPkg ignore True (Pkg n1 v1) (Pkg n2 v2)
diffPkg _ p1 p2 | pkgName p1 == pkgName p2 && pkgIdent p1 /= pkgIdent p2 = Just $ PkgArch p1 p2
diffPkg _ _ _ = Nothing

compareNames :: Package -> Package -> Ordering
compareNames p1 p2 = compare (pkgName p1) (pkgName p2)

-- infixr 4 <.>
-- (<.>) :: Text -> Text -> Text
-- s <.> t = s <> "." <> t

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,0))
#else
warning :: String -> IO ()
warning = hPutStrLn stderr
#endif

-- borrowed straight from extra:Control.Monad.Extra
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where f x xs = do x' <- op x; if null x' then xs else do xs' <- xs; return $ x'++xs'

data DiffSum = DS {updateSum, newSum, delSum, archSum :: Int}

emptyDS :: DiffSum
emptyDS = DS 0 0 0 0

summary :: [PackageDiff] -> DiffSum
summary =
  foldl' countDiff emptyDS
  where
    countDiff :: DiffSum -> PackageDiff -> DiffSum
    countDiff ds pd =
      case pd of
        PkgUpdate {} -> ds {updateSum = updateSum ds + 1}
        PkgAdd _ -> ds {newSum = newSum ds + 1}
        PkgDel _ -> ds {delSum = delSum ds + 1}
        PkgArch {} -> ds {archSum = archSum ds + 1}
