firstHalf list = take (div (length list) 2) list

inFirstHalf n list = n `elem` firstHalf list
