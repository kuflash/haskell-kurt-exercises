nyOffice name =
  nameText ++ ": New York, Postcode: 789"
  where
    nameText = (fst name) ++ " " ++ (snd name)

sfOffice name =
  if lastName < "Л"
    then nameText ++ " - San Francisco, Postcode: 1234"
    else nameText ++ " - San Francisco, Postcode: 1010"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

renoOffice name =
  nameText ++ " - Reno, Postcode: 456"
  where
    nameText = (snd name)

columbiaOffice name =
  nameText ++ " - Columbia, Postcode: 333"
  where
    nameText = "Dear, " ++ (snd name) ++ " " ++ (fst name)

getLocationFn location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "cb" -> columbiaOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location =
  locationFn name
  where
    locationFn = getLocationFn location
