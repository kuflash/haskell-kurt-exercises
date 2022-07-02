toPart recipient = "Dear " ++ recipient ++ "!\n"

bodyPart title = "Thank you for bought " ++ title ++ "!\n"

fromPart author = "Best regards,\n " ++ author

createEmail recipient title author =
  toPart recipient
    ++ bodyPart title
    ++ fromPart author

main = do
  putStrLn "Who is a recipient?"
  recipient <- getLine
  putStrLn "Book title:"
  title <- getLine
  putStrLn "Who is an author?"
  author <- getLine
  putStrLn (createEmail recipient title author)
