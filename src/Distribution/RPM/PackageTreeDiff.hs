{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A library for pkgtreediff for comparing trees of rpm packages
module Distribution.RPM.PackageTreeDiff
  (NVRA(..),
   readNVRA,
   showNVRA,
   showPkgIdent,
   showPkgVerRel,
   RPMPkgDiff(..),
   diffPkgs,
   diffPkg,
   Ignore(..),
   Mode(..),
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Maybe
import Data.RPM.VerRel
import Data.RPM.NVRA
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

-- | Mode describes the kind of summary generated by compareDirs
data Mode = AutoSummary | NoSummary | ShowSummary | Added | Deleted | Updated | RST
  deriving Eq

-- | Ignore describes how comparison is done
data Ignore = IgnoreNone    -- ^ do not ignore version or release
            | IgnoreRelease -- ^ ignore differences in release
            | IgnoreVersion -- ^ ignore differences in version
  deriving Eq

-- eqVR True ignore release
eqVR :: Ignore -> VerRel -> VerRel -> Bool
eqVR IgnoreNone vr vr' = vr == vr'
eqVR IgnoreRelease (VerRel v _) (VerRel v' _) = v == v'
eqVR IgnoreVersion _ _ = True

-- | RPMPkgDiff type encodes how a particular rpm package differs between trees
data RPMPkgDiff = PkgUpdate NVRA NVRA
                | PkgAdd NVRA
                | PkgDel NVRA
                | PkgArch NVRA NVRA
  deriving Eq

-- | Compare two lists of packages NVRAs
diffPkgs :: Ignore -> [NVRA] -> [NVRA] -> [RPMPkgDiff]
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

-- | Compare two rpms of a package
diffPkg :: Ignore -> NVRA-> NVRA-> Maybe RPMPkgDiff
diffPkg ignore p1 p2 | showPkgIdent p1 == showPkgIdent p2 =
                         if eqVR ignore (rpmVerRel p1) (rpmVerRel p2)
                         then Nothing
                         else Just $ PkgUpdate p1 p2
diffPkg _ p1 p2 | rpmName p1 == rpmName p2 && showPkgIdent p1 /= showPkgIdent p2 = Just $ PkgArch p1 p2
diffPkg _ _ _ = Nothing

compareNames :: NVRA -> NVRA -> Ordering
compareNames p1 p2 = compare (rpmName p1) (rpmName p2)
