{-# LANGUAGE OverloadedStrings #-}

module DB (createConnection, addToDB, readDB, deleteById) where

import           Prelude hiding (Word)
import           GHC.Generics
import           Database.PostgreSQL.Simple
import qualified Data.Text as T

type Word = (String, String, String, Int)

createConnection :: IO Connection
createConnection =
  connect $ defaultConnectInfo { connectUser = "bot", connectDatabase = "bot" }

addToDB :: Connection -> Word -> IO ()
addToDB con (a, b, c, i) = do
  execute con "INSERT INTO words VALUES (?, ?, ?, ?)" (a, b, c, i)
  return ()

readDB :: Connection -> IO [Word]
readDB con = query_ con "SELECT * FROM words"

deleteById :: Connection -> Int -> IO ()
deleteById con id_ = do
  execute con "DELETE FROM words WHERE id_ = ?" [id_]
  return ()
