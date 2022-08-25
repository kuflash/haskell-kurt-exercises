module Main (main) where

import Control.Applicative
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesBorrowed :: Int
  }

data User = User {userId :: Int, userName :: String}

instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool,
        ".) ",
        name tool,
        "\ndescription: ",
        description tool,
        "\nlast returned: ",
        show $ lastReturned tool,
        "\ntimes borrowed: ",
        show $ timesBorrowed tool,
        "\n"
      ]

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

toolsDB :: String
toolsDB = "tools.db"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

addUser :: String -> IO ()
addUser userName =
  withConn
    toolsDB
    $ \conn -> do
      execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
      putStrLn "User added"

addTool :: String -> String -> IO ()
addTool name description =
  withConn
    toolsDB
    $ \conn -> do
      currentDay <- utctDay <$> getCurrentTime
      execute conn "INSERT INTO tools (name, description, lastReturned, timesBorrowed) VALUES (?,?,?,?)" (name, description, currentDay, 0 :: Int)
      putStrLn "Tool added"

checkout :: Int -> Int -> IO ()
checkout userId toolId =
  withConn
    toolsDB
    $ \conn -> do
      execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)" (userId, toolId)
      putStrLn "Tool checked"

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool {lastReturned = date, timesBorrowed = 1 + timesBorrowed tool}

updateToolOrWarn :: Maybe Tool -> IO ()
updateToolOrWarn Nothing = putStrLn "No tool id was found"
updateToolOrWarn (Just tool) =
  withConn
    toolsDB
    $ \conn ->
      do
        let q = mconcat ["UPDATE tools SET ", "lastReturned = ?,", "timesBorrowed = ? ", "WHERE id = ?;"]
        execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
        putStrLn "Tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId =
  withConn
    toolsDB
    $ \conn -> do
      tool <- selectTool conn toolId
      currentDay <- utctDay <$> getCurrentTime
      let updatedTool = updateTool <$> tool <*> pure currentDay
      updateToolOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId =
  withConn
    toolsDB
    $ \conn ->
      execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

printUsers :: IO ()
printUsers =
  withConn
    toolsDB
    $ \conn -> do
      resp <- query_ conn "SELECT * FROM users;" :: IO [User]
      mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q =
  withConn
    toolsDB
    $ \conn -> do
      resp <- query_ conn q :: IO [Tool]
      mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailableTools :: IO ()
printAvailableTools =
  printToolQuery $
    mconcat
      [ "SELECT * FROM tools ",
        "WHERE id NOT IN ",
        "(SELECT tool_id from checkedout);"
      ]

printCheckedoutTools :: IO ()
printCheckedoutTools =
  printToolQuery $
    mconcat
      [ "SELECT * FROM tools ",
        "WHERE id IN ",
        "(SELECT tool_id from checkedout);"
      ]

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter a new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  putStrLn "Enter an user id"
  userId <- pure read <*> getLine
  putStrLn "Enter an tool id"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  putStrLn "Enter an tool id"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand "users" = printUsers >> main
performCommand "tools" = printTools >> main
performCommand "adduser" = promptAndAddUser >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin" = promptAndCheckin >> main
performCommand "in" = printAvailableTools >> main
performCommand "out" = printCheckedoutTools >> main
performCommand "quit" = putStrLn "Goodbye!"
performCommand _ = putStrLn "Command not found" >> main

main :: IO ()
main = do
  putStrLn "Enter a command"
  command <- getLine
  performCommand command
