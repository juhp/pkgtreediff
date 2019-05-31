{-# LANGUAGE CPP #-}

import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
  , (<$>), (<*>)
#endif
  )
import Control.Concurrent.Async (concurrently)
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
import System.Directory (listDirectory)

import SimpleCmdArgs

import Paths_pkgtreediff (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees" $
    compareDirs <$> ignoreRelease <*> modeOpt  <*> strArg "URL/DIR1" <*> strArg "URL/DIR2"

data Mode = Default | Added | Removed | Updated
  deriving Eq

modeOpt :: Parser Mode
modeOpt = flagWith' Added 'N' "new" "Show only new packages" <|>
          flagWith' Removed 'D' "removed" "Show only removed packages" <|>
          flagWith Default Updated 'U' "updated" "Show only updated packages"

ignoreRelease :: Parser Bool
ignoreRelease = switchWith 'R' "ignore-release" "Only show version changes (ignore release)"

compareDirs :: Bool -> Mode -> String -> String -> IO ()
compareDirs ignoreRel mode tree1 tree2 = do
  (ps1,ps2) <- getTrees tree1 tree2
  mapM_ T.putStrLn . mapMaybe (showPkgDiff mode) $ diffPkgs ignoreRel ps1 ps2
  where
    getTrees :: String -> String -> IO ([Package],[Package])
    getTrees t1 t2 =
      if t1 == t2 then error "Trees must be different"
      else do
        mmgr <- if any isHttp [t1,t2] then Just <$> httpManager else return Nothing
        concurrently (readPackages mmgr t1) (readPackages mmgr t2)

    readPackages mmgr loc = do
      fs <- if isHttp loc
            then filter (not <$> \ f -> "/" `T.isSuffixOf` f || "?" `T.isPrefixOf` f) <$> httpDirectory (fromJust mmgr) loc
            else sort . map T.pack <$> listDirectory loc
      return $ map readPkg fs

    isHttp :: String -> Bool
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

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

showNameArch :: Package -> Text
showNameArch (Pkg na _) = nameArch na

readPkg :: Text -> Package
readPkg t =
  if compnts < 3 then error $ T.unpack $ "Malformed rpm package name: " <> t
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
showPkgDiff Default (PkgAdd p) = Just $ "+ " <> showNameArch p
showPkgDiff Default (PkgDel p) = Just $ "- " <> showNameArch p
showPkgDiff Default (PkgUpdate na v v') = Just $ nameArch na <> ": " <> verRel v <> " -> " <> verRel v'
showPkgDiff Default (PkgArch n (a,v) (a',v')) = Just $ n <> ": " <> verRel v <.> a <> " -> " <> verRel v' <.> a'
showPkgDiff Added (PkgAdd p) = Just $ showNameArch p
showPkgDiff Removed (PkgDel p) = Just $ showNameArch p
showPkgDiff Updated (PkgUpdate na v v') = Just $ nameArch na <> ": " <> verRel v <> " -> " <> verRel v'
showPkgDiff Updated (PkgArch n (a,v) (a',v')) = Just $ n <> ": " <> verRel v <.> a <> " -> " <> verRel v' <.> a'
showPkgDiff _ _ = Nothing

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
