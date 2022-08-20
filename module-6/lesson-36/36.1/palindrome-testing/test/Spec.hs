import Data.Char (isPunctuation, isSpace)
import Data.Text as T
import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances ()

prop_punctuationInvariant :: Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_whitespaceInvariant :: Text -> Bool
prop_whitespaceInvariant text = preprocess text == preprocess noSpaceText
  where
    noSpaceText = T.filter (not . isSpace) text

prop_caseInvariant :: Text -> Bool
prop_caseInvariant text = preprocess text == preprocess lowerCaseText
  where
    lowerCaseText = T.toLower text

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whitespaceInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_caseInvariant
  putStrLn "Done"
