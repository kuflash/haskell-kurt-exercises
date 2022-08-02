type FirstName = String

type MiddleName = String

type LastName = String

data Sex = Male | Female

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ "  " ++ l

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient donator receiver = canDonateTo (bloodType donator) (bloodType receiver)
