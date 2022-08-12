{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

-- Download file from
-- https://archive.org/download/marc_oregon_summit_records/catalog_files/ohsu_ncnm_wscc_bibs.mrc
-- for testing the program

type Author = T.Text

type Title = T.Text

data Book = Book {author :: Author, title :: Title} deriving (Show)

type Html = T.Text

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

type MarcDirectoryRaw = B.ByteString

type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

data FieldMetadata = FieldMetadata
  { tag :: T.Text,
    fieldLength :: Int,
    fieldStart :: Int
  }
  deriving (Show)

leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

fieldDelimeter :: Char
fieldDelimeter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

additionalTitleSubfield :: Char
additionalTitleSubfield = 'b'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where
    remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
    then []
    else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry =
  FieldMetadata textTag theLength theStart
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata =
  E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if length results < 1
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if results == []
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subFields = T.split (== fieldDelimeter) rawField
    results = filter ((== subfield) . T.head) subFields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record =
  lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAdditionalTitle :: MarcRecordRaw -> Maybe Title
lookupAdditionalTitle = lookupValue titleTag additionalTitleSubfield

lookupFullTitle :: MarcRecordRaw -> Maybe Title
lookupFullTitle record = Just (mconcat [title, additionalTitle])
  where
    title = fromMaybe "" (lookupTitle record)
    additionalTitle = fromMaybe "" (lookupAdditionalTitle record)

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength (getLeader marcStream)

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream =
  if marcStream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupFullTitle records
    authors = map lookupAuthor records

pairToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairToBooks pairs =
  map
    (\(title, author) -> Book {title = fromJust title, author = fromJust author})
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords count = booksToHtml . pairToBooks . take count . marcToPairs

book1 :: Book
book1 = Book {title = "Charlotte’s Web", author = "E.B. White"}

book2 :: Book
book2 = Book {title = "Mieko and the Fifth Treasure", author = "Eleanor Coerr"}

book3 :: Book
book3 = Book {title = "The Outsiders", author = "S.E. Hinton"}

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n",
      "<head><title>Books</title>",
      "<meta charset='utf-8'/>",
      "</head>\n",
      "<body>\n",
      booksHtml,
      "</body>\n",
      "</html>"
    ]
  where
    booksHtml = (mconcat . map (bookToHtml)) books

books :: [Book]
books = [book1, book2, book3]

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed

-- main = TIO.writeFile "books.html" (booksToHtml books)
