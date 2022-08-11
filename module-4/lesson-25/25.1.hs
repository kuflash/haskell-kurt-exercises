import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  file <- BC.readFile filePath
  let fileSize = BC.length file
  let charsCount = T.length (E.decodeUtf8 file)
  putStrLn (mconcat ["File size: ", show fileSize, "; Chars count: ", show charsCount])
