rotNEncoder :: (Bounded a, Enum a) => Int -> a -> a
rotNEncoder alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = halfAlphabet + fromEnum c
    rotation = offset `mod` alphabetSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset =
      if even halfAlphabet
        then halfAlphabet + fromEnum c
        else 1 + halfAlphabet + fromEnum c
    rotation = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar char = rotNEncoder sizeOfAlphabet char
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotEncoder :: String -> String
rotEncoder str = map encoder str
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    encoder = rotNEncoder alphabetSize

rotDecoder :: String -> String
rotDecoder str = map decoder str
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    decoder = rotNDecoder alphabetSize

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor listA listB = map xorPair (zip listA listB)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size = length bits
    indices = [size -1, size -2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits textBits)
  where
    padBits = map charToBits pad
    textBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prgn :: Int -> Int -> Int -> Int -> Int
prgn a b maxNumber seed = (a * seed + b) `mod` maxNumber

data StreamCipher = StreamCipher Int

generatePad :: Int -> String
generatePad padLength = map bitsToChar bitStream
  where
    intStream = map (\i -> prgn padLength i maxBound (padLength + 1 - i)) [0, 1 .. padLength]
    bitStream = map intToBits intStream

instance Cipher StreamCipher where
  encode (StreamCipher n) text = applyOTP (generatePad padLength) text
    where
      textLength = length text
      padLength = textLength + n

  decode (StreamCipher n) text = applyOTP (generatePad padLength) text
    where
      textLength = length text
      padLength = textLength + n
