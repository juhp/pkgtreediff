import Data.Maybe (isJust)
import SimpleCmd
import System.Directory (findExecutable)
import System.IO

program :: [String] -> IO ()
program test = do
  timeIO $ cmdLog "pkgtreediff" test
  putStrLn ""

tests :: Bool -> [[String]]
tests dnfrepo =
  [
    ["-r", "test/files/35", "test/files/36"]
  ]
  ++
  [
    -- FIXME until native repoquerying use dnf-repo
    ["--ignore-release",
     "dnf-repo -q -d * -u http://download.fedoraproject.org/pub/fedora/linux/releases/35/Everything/x86_64/os/ repoquery --qf %{name}-%{version}-%{release}.%{arch}",
     "dnf-repo -q -d * -u http://download.fedoraproject.org/pub/fedora/linux/releases/36/Everything/x86_64/os/ repoquery --qf %{name}-%{version}-%{release}.%{arch}"]
  | dnfrepo]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  have_dnfrepo <- findExecutable "dnf-repo"
  let activeTests = tests $ isJust have_dnfrepo
  mapM_ program activeTests
  putStrLn $ "\n" ++ show (length activeTests) ++ " command tests run"
