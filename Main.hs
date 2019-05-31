{-# LANGUAGE CPP #-}

import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
  , (<$>), (<*>)
#endif
  )
import Control.Concurrent.Async (concurrently)
import Control.Monad
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
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
-- for warning
import System.IO (hPutStrLn, stderr)

import SimpleCmd (error', {-warning-})
import SimpleCmdArgs

import Paths_pkgtreediff (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees" $
    compareDirs <$> recursiveOpt <*> ignoreRelease <*> modeOpt  <*> strArg "URL/DIR1" <*> strArg "URL/DIR2"

data Mode = Default | Added | Removed | Updated
  deriving Eq

modeOpt :: Parser Mode
modeOpt = flagWith' Added 'N' "new" "Show only added packages" <|>
          flagWith' Removed 'D' "removed" "Show only removed packages" <|>
          flagWith Default Updated 'U' "updated" "Show only updated packages"

ignoreRelease :: Parser Bool
ignoreRelease = switchWith 'R' "ignore-release" "Only show version changes (ignore release)"

recursiveOpt :: Parser Bool
recursiveOpt = switchWith 'r' "recursive" "Recursive down into subdirectories"

compareDirs :: Bool -> Bool -> Mode -> String -> String -> IO ()
compareDirs recursive ignoreRel mode tree1 tree2 = do
  (ps1,ps2) <- getTrees tree1 tree2
  mapM_ T.putStrLn . mapMaybe (showPkgDiff mode) $ diffPkgs ignoreRel ps1 ps2
  where
    getTrees :: String -> String -> IO ([Package],[Package])
    getTrees t1 t2 = do
      when (t1 == t2) $ warning "Comparing the same tree!"
      let (isUrl1,isUrl2) = (isHttp t1, isHttp t2)
      mmgr <- if isUrl1 || isUrl2 then Just <$> httpManager else return Nothing
      concurrently (readPackages isUrl1 mmgr t1) (readPackages isUrl2 mmgr t2)

    readPackages isUrl mmgr loc =
      map readPkg <$> (if isUrl then httpPackages recursive (fromJust mmgr) else dirPackages) loc

    httpPackages recurse mgr url = do
      exists <- httpExists mgr url
      fs <- if exists
            then filter (not <$> \ f -> "/" `T.isPrefixOf` f || "?" `T.isPrefixOf` f || f == "../") <$> httpDirectory mgr url
            else error' $ "Could not get " <> url
      if recurse && all isDir fs then concatMapM (httpPackages False mgr) (map ((url </>) . T.unpack) fs) else return fs

    dirPackages dir = do
      fs <- sort <$> listDirectory dir
      alldirs <- mapM doesDirectoryExist fs
      if recursive && and alldirs then concatMapM dirPackages (map (dir </>) fs) else return $ map T.pack fs

    isHttp :: String -> Bool
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

    isDir = ("/" `T.isSuffixOf`)

type Arch = Text

data NameArch = NA {name :: Text, _arch :: Arch}
  deriving Eq

nameArch :: NameArch -> Text
nameArch (NA n a) = n <.> a

data VersionRelease = VerRel Text Text
  deriving Eq

-- eqVR True ignore release
eqVR :: Bool -> VersionRelease -> VersionRelease -> Bool
eqVR False vr vr' = vr == vr'
eqVR True (VerRel v _) (VerRel v' _) = v == v'

verRel :: VersionRelease -> Text
verRel (VerRel v r) = v <> "-" <> r

data Package = Pkg {_pkgNameArch :: NameArch, _pkgVerrel :: VersionRelease}

showPkg :: Package -> Text
showPkg (Pkg na vr) = nameArch na <> "  " <> verRel vr

readPkg :: Text -> Package
readPkg t =
  if compnts < 3 then error' $ T.unpack $ "Malformed rpm package name: " <> t
  else Pkg (NA (intrclt ns) arch) (VerRel ver rel)
  where
    (nvr',arch) = T.breakOnEnd "." $ fromMaybe t $ T.stripSuffix ".rpm" t
    pieces = reverse $ T.splitOn "-" $ T.dropEnd 1 nvr'
    compnts = length pieces
    (rel:ver:emaN) = pieces
    ns = reverse emaN

intrclt :: [Text] -> Text
intrclt = T.intercalate "-"

data PackageDiff = PkgUpdate NameArch VersionRelease VersionRelease
                 | PkgAdd Package
                 | PkgDel Package
                 | PkgArch Text (Text,VersionRelease) (Text,VersionRelease)

showPkgDiff :: Mode -> PackageDiff -> Maybe Text
showPkgDiff Default (PkgAdd p) = Just $ "+ " <> showPkg p
showPkgDiff Default (PkgDel p) = Just $ "- " <> showPkg p
showPkgDiff Default (PkgUpdate na v v') = Just $ indent $ nameArch na <> ": " <> verRel v <> " -> " <> verRel v'
showPkgDiff Default (PkgArch n (a,v) (a',v')) = Just $ indent $ n <> ": " <> verRel v <.> a <> " -> " <> verRel v' <.> a'

showPkgDiff Added (PkgAdd p) = Just $ showPkg p
showPkgDiff Removed (PkgDel p) = Just $ showPkg p
showPkgDiff Updated (PkgUpdate na v v') = Just $ nameArch na <> ": " <> verRel v <> " -> " <> verRel v'
showPkgDiff Updated (PkgArch n (a,v) (a',v')) = Just $ n <> ": " <> verRel v <.> a <> " -> " <> verRel v' <.> a'
showPkgDiff _ _ = Nothing

indent :: Text -> Text
indent = (" " <>)

diffPkgs :: Bool -> [Package] -> [Package] -> [PackageDiff]
diffPkgs _ [] [] = []
diffPkgs ignoreRel (p:ps) [] = PkgDel p : diffPkgs ignoreRel ps []
diffPkgs ignoreRel [] (p:ps) = PkgAdd p : diffPkgs ignoreRel [] ps
diffPkgs ignoreRel (p1:ps1) (p2:ps2) =
  case comparePkgs p1 p2 of
    LT -> PkgDel p1 : diffPkgs ignoreRel ps1 (p2:ps2)
    EQ -> let diff = diffPkg ignoreRel p1 p2
              diffs = diffPkgs ignoreRel ps1 ps2
          in if isJust diff then fromJust diff : diffs else diffs
    GT -> PkgAdd p2 : diffPkgs ignoreRel (p1:ps1) ps2

diffPkg :: Bool -> Package -> Package -> Maybe PackageDiff
diffPkg ignoreRel (Pkg na1 v1) (Pkg na2 v2) | na1 == na2 =
                                           if eqVR ignoreRel v1 v2
                                           then Nothing
                                           else Just $ PkgUpdate na1 v1 v2
diffPkg _ (Pkg (NA n1 a1) v1) (Pkg (NA n2 a2) v2)
  | n1 == n2 && "noarch" `elem` [a1,a2] = Just $ PkgArch n1 (a1,v1) (a2,v2)
diffPkg _ _ _ = Nothing

comparePkgs :: Package -> Package -> Ordering
comparePkgs (Pkg na1 _) (Pkg na2 _) = compare (name na1) (name na2)

infixr 4 <.>
(<.>) :: Text -> Text -> Text
s <.> t = s <> "." <> t

-- from next simple-cmd
warning :: String -> IO ()
warning = hPutStrLn stderr

-- borrowed straight from extra:Control.Monad.Extra
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where f x xs = do x' <- op x; if null x' then xs else do xs' <- xs; return $ x'++xs'
