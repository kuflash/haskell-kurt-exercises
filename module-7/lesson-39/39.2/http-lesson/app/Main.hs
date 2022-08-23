module Main (main) where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

myToken :: BC.ByteString
myToken = "" -- put your token here

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.go" -- invalid host for getting error

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort 443 $
              defaultRequest

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure False $
            setRequestPort 80 $
              defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      BC.putStrLn "Response was saved into file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else BC.putStrLn $ statusMessage $ getResponseStatus response
