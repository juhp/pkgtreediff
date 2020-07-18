{-# LANGUAGE CPP #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
    (<|>),
#endif
#if !MIN_VERSION_base(4,8,0)
    (<$>), (<*>)
#endif
  )
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import SimpleCmdArgs

import Distribution.RPM.PackageTreeDiff
import Paths_pkgtreediff (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees or instances" $
    compareDirs <$> recursiveOpt <*> ignoreVR <*> ignoreArch <*> modeOpt  <*> optional patternOpt <*> timeoutOpt <*> strArg "URL|DIR|FILE|CMD1" <*> strArg "URL|DIR|FILE|CMD2"
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

    timeoutOpt :: Parser Int
    timeoutOpt = optionalWith auto 't' "timeout" "SECONDS" "Maximum seconds to wait for http response before timing out (default 30)" 30
