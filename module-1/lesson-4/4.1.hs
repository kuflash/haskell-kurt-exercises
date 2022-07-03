compareLastNames name1 name2 =
  if lastName1 > lastName2
    then GT
    else
      if lastName1 < lastName2
        then LT
        else
          if firstName1 > firstName2
            then GT
            else
              if firstName1 < firstName2
                then LT
                else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2

improvedCompareLastNames name1 name2 =
  if lastNameCompareResult == EQ
    then firstNameCompareResult
    else lastNameCompareResult
  where
    lastNameCompareResult = compare (snd name1) (snd name2)
    firstNameCompareResult = compare (fst name1) (fst name2)
