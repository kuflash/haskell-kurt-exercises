getRequestUrl host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

getBookUrl = getRequestUrl "http://example.com" "1tre312q" "book"
