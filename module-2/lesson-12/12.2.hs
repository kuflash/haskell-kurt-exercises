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
showName (Name f l) = l ++ ", " ++ f
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ "  " ++ l

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showHeight :: Int -> String
showHeight value = show value ++ "cm"

showWeight :: Int -> String
showWeight value = show value ++ "kg"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

showProperty :: String -> String -> String
showProperty title value = title ++ ": " ++ value ++ "\n"

patientSummary :: Patient -> String
patientSummary patient =
  "**********\n"
    ++ showProperty "Patient name" (showName (name patient))
    ++ showProperty "Sex" (showSex (sex patient))
    ++ showProperty "Age" (show (age patient))
    ++ showProperty "Height" (showHeight (height patient))
    ++ showProperty "Weight" (showWeight (weight patient))
    ++ showProperty "BloodType" (showBloodType (bloodType patient))
    ++ "**********\n"

johnSmith =
  Patient
    { name = Name "Jhon" "Smith",
      sex = Male,
      age = 46,
      height = 185,
      weight = 95,
      bloodType = BloodType B Pos
    }
