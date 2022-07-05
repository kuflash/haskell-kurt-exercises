genIfEvenX x =
  \f ->
    if even x
      then f x
      else x
