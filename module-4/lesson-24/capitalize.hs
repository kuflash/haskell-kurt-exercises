import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  fileData <- TI.readFile fileName
  let capitalizedData = T.toUpper fileData
  TI.writeFile fileName capitalizedData
