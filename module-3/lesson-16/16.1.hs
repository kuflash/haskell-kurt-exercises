type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName

data Author = Author Name

data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletDescription :: String,
    contact :: String
  }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0
