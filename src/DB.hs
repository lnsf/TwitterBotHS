{-# LANGUAGE OverloadedStrings #-}

module DB (createConnection, addToDB, readDB, deleteById, Word) where

import           Types
import           GHC.Generics
import           Database.PostgreSQL.Simple
import qualified Data.Text as T

createConnection :: IO Connection
createConnection =
  connect $ defaultConnectInfo { connectUser = "bot", connectDatabase = "bot" }

addToDB :: Connection -> DBEntry -> IO ()
addToDB con (a, b, c, i) = do
  execute con "INSERT INTO words VALUES (?, ?, ?, ?)" (a, b, c, i)
  return ()

readDB :: Connection -> IO [DBEntry]
readDB con = query_ con "SELECT * FROM words"

deleteById :: Connection -> Int -> IO ()
deleteById con id_ = do
  execute con "DELETE FROM words WHERE id_ = ?" [id_]
  return ()
