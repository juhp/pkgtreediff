{-# LANGUAGE CPP #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
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
    compareDirs <$> strArg "TREE1" <*> strArg "TREE1"

compareDirs :: String -> String -> IO ()
compareDirs url1 url2 = do
  mgr <- httpManager
  ps1 <- readPackages mgr url1
  ps2 <- readPackages mgr url2
  mapM_ (T.putStrLn . showPkgDiff) $ diffPkgs ps1 ps2
  where
    readPackages mgr url =
      map readPkg . filter (/= "../") <$> httpDirectory mgr url

data Package = Pkg {pkgNameArch :: Text, _pkgVerrel :: Text}

-- showPkg :: Package -> Text
-- showPkg (Pkg na v) = na <> " " <> v

readPkg :: Text -> Package
readPkg t =
  Pkg (intrclt ns <> "." <> arch) (intrclt vrs)
  where
    (nvr',arch) = T.breakOnEnd "." $ fromMaybe t $ T.stripSuffix ".rpm" t
    pieces = T.splitOn "-" $ T.dropEnd 1 nvr'
    compnts = length pieces
    (ns,vrs) = 
      if compnts < 3 
      then error $ T.unpack $ "Malformed package " <> t
      else splitAt (compnts - 2) pieces

intrclt :: [Text] -> Text
intrclt = T.intercalate "-"

--putPkg :: Package -> IO ()
--putPkg (Pkg n _) = T.putStrLn n

data PackageDiff = PkgUpdate Text Text Text
                 | PkgAdd Package
                 | PkgDel Package

showPkgDiff :: PackageDiff -> Text
showPkgDiff (PkgAdd p) = "+ " <> pkgNameArch p
showPkgDiff (PkgDel p) = "- " <> pkgNameArch p
showPkgDiff (PkgUpdate na v v') = na <> ": " <> v <> " -> " <> v'

diffPkgs :: [Package] -> [Package] -> [PackageDiff]
diffPkgs [] [] = []
diffPkgs (p:ps) [] = PkgDel p : diffPkgs ps []
diffPkgs [] (p:ps) = PkgAdd p : diffPkgs [] ps
diffPkgs (p1:ps1) (p2:ps2) =
  case comparePkgs p1 p2 of
    LT -> PkgDel p1 : diffPkgs ps1 (p2:ps2)
    EQ -> let diff = diffPkg p1 p2 
              diffs = diffPkgs ps1 ps2
          in if isJust diff then fromJust diff : diffs else diffs
    GT -> PkgAdd p2 : diffPkgs (p1:ps1) ps2

diffPkg :: Package -> Package -> Maybe PackageDiff
diffPkg (Pkg na1 v1) (Pkg na2 v2) | na1 == na2 = 
                                  if v1 == v2 then Nothing
                                  else Just $ PkgUpdate na1 v1 v2
diffPkg _ _ = Nothing

comparePkgs :: Package -> Package -> Ordering
comparePkgs (Pkg na1 _) (Pkg na2 _) = compare na1 na2
