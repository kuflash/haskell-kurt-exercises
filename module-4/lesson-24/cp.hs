import qualified Data.Text.IO as TI
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let sourceFileName = args !! 0
  let targetFileName = args !! 1
  fileData <- TI.readFile sourceFileName
  TI.writeFile targetFileName fileData
