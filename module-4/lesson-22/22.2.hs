import qualified Data.Map as Map
import Data.Maybe

quotes :: Map.Map Int String
quotes =
  Map.fromList
    [ (1, "A rose by any other name would smell as sweet. - William Shakespeare"),
      (2, "All that glitters is not gold. - William Shakespeare"),
      (3, "All the world’s a stage, and all the men and women merely players. - William Shakespeare"),
      (4, "Ask not what your country can do for you; ask what you can do for your country. - John Kennedy"),
      (5, "Ask, and it shall be given you; seek, and you shall find. - the Bible"),
      (6, "Eighty percent of success is showing up. - Woody Allen"),
      (7, "For those to whom much is given, much is required. - the Bible"),
      (8, "Genius is one percent inspiration and ninety-nine percent perspiration. - Thomas Edison"),
      (9, "Hell is other people. - Jean-Paul Sartre"),
      (10, "If at first you don’t succeed, try, try again. - W. E. Hickson"),
      (11, "If you are going through hell, keep going. - Winston Churchill"),
      (12, "If you want something done right, do it yourself. - Charles-Guillaume Étienne"),
      (13, "If you want something said, ask a man; if you want something done, ask a woman. - Margaret Thatcher"),
      (14, "Keep your friends close, but your enemies closer. - Michael Corleone"),
      (15, "No one can make you feel inferior without your consent. - Eleanor Roosevelt")
    ]

getQuote :: Int -> String
getQuote n = fromMaybe "Not found" (Map.lookup n quotes)

helloMessage :: String
helloMessage = "Enter a quote number from 1 to 15 or 'n' for ending the program"

readUserInput :: [String] -> [String]
readUserInput [] = []
readUserInput ("n" : rest) = []
readUserInput (quoteNumber : rest) = getQuote (read quoteNumber) : helloMessage : readUserInput rest

main :: IO ()
main = do
  putStrLn helloMessage
  userInput <- getContents
  mapM_ putStrLn (readUserInput (lines userInput))
