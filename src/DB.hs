{-# LANGUAGE OverloadedStrings #-}

module DB (createConnection, addToDB, readDB, deleteById, Word) where

import Lib
import           Data.Block
import           GHC.Generics
import           Database.PostgreSQL.Simple
import Prelude hiding (id)
import qualified Data.Text as T

createConnection :: IO Connection
createConnection = connect
  $ defaultConnectInfo { connectUser = "bot"
                       , connectDatabase = "bot"
                       , connectPassword = "postgres"
                       }

addToDB :: Connection -> Block -> IO ()
addToDB con b = do
  execute con "INSERT INTO words VALUES (?, ?, ?, ?)" ((get1 . block) b, (get2 . block) b, (get3 . block) b, id b)
  return ()

readDB :: Connection -> IO [Block]
readDB con = toBlock <$> query_ con "SELECT * FROM words"
  where
    toBlock :: [(T.Text, T.Text, T.Text, Integer)] -> [Block]
    toBlock [] = []
    toBlock ((a, b, c, i):xs) = Block (a, b, c) i : toBlock xs

deleteById :: Connection -> Integer -> IO ()
deleteById con id_ = do
  execute con "DELETE FROM words WHERE id_ = ?" [id_]
  return ()
