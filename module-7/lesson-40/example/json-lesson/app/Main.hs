module Main (main) where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.Text as T
import GHC.Generics
import Lib

data NOAAResult = NOAAResult
  { uid :: T.Text,
    mindate :: T.Text,
    maxdate :: T.Text,
    name :: T.Text,
    datacoverage :: Int,
    resultId :: T.Text
  }
  deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
      <*> v .: "mindate"
      <*> v .: "maxdate"
      <*> v .: "name"
      <*> v .: "datacoverage"
      <*> v .: "id"

data Resultset = Resultset
  { offset :: Int,
    count :: Int,
    limit :: Int
  }
  deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata {resultset :: Resultset} deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse {metadata :: Metadata, results :: [NOAAResult]} deriving (Show, Generic)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = Prelude.putStrLn "Error of data loading"
printResults (Just r) = forM_ r (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
