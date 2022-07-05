getRequestUrl host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host =
  \apiKey resource id ->
    getRequestUrl host apiKey resource id

genApiRequestBuilder hostBuilder apiKey resource =
  \id ->
    hostBuilder apiKey resource id

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1tre312q"
