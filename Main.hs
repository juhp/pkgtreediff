{-# LANGUAGE CPP #-}

import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
  , (<$>), (<*>)
#endif
  )
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP.Directory
import SimpleCmdArgs

import Paths_pkgtreediff (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees" $
    compareDirs <$> showOpt <*> modeOpt  <*> strArg "TREE1" <*> strArg "TREE1"

data Mode = Default | New | Removed | Same
  deriving Eq

modeOpt :: Parser Mode
modeOpt = flagWith' New 'A' "add" "Show only new packages" <|>
          flagWith' Removed 'D' "delete" "Show only removed packages" <|>
          flagWith Default Same 'S' "same" "Show only updated packages"

showOpt :: Parser Bool
showOpt = switchWith 'V' "only-version" "Only show version changes (ignore release)"

compareDirs :: Bool -> Mode -> String -> String -> IO ()
compareDirs onlyVer mode url1 url2 = do
  mgr <- httpManager
  ps1 <- readPackages mgr url1
  ps2 <- readPackages mgr url2
  mapM_ T.putStrLn . mapMaybe (showPkgDiff mode) $ diffPkgs onlyVer ps1 ps2
  where
    readPackages mgr url =
      map readPkg . filter (not <$> \ f -> "/" `T.isSuffixOf` f || "?" `T.isPrefixOf` f) <$> httpDirectory mgr url

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
showPkgDiff New (PkgAdd p) = Just $ showNameArch p
showPkgDiff Removed (PkgDel p) = Just $ showNameArch p
showPkgDiff Same (PkgUpdate na v v') = Just $ nameArch na <> ": " <> verRel v <> " -> " <> verRel v'
showPkgDiff Same (PkgArch n (a,v) (a',v')) = Just $ n <> ": " <> verRel v <.> a <> " -> " <> verRel v' <.> a'
showPkgDiff _ _ = Nothing

diffPkgs :: Bool -> [Package] -> [Package] -> [PackageDiff]
diffPkgs _ [] [] = []
diffPkgs onlyVer (p:ps) [] = PkgDel p : diffPkgs onlyVer ps []
diffPkgs onlyVer [] (p:ps) = PkgAdd p : diffPkgs onlyVer [] ps
diffPkgs onlyVer (p1:ps1) (p2:ps2) =
  case comparePkgs p1 p2 of
    LT -> PkgDel p1 : diffPkgs onlyVer ps1 (p2:ps2)
    EQ -> let diff = diffPkg onlyVer p1 p2
              diffs = diffPkgs onlyVer ps1 ps2
          in if isJust diff then fromJust diff : diffs else diffs
    GT -> PkgAdd p2 : diffPkgs onlyVer (p1:ps1) ps2

diffPkg :: Bool -> Package -> Package -> Maybe PackageDiff
diffPkg onlyVer (Pkg na1 v1) (Pkg na2 v2) | na1 == na2 =
                                           if eqVR onlyVer v1 v2
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
