{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
    ( createConnection
    , addToDB
    , readDB
    , getLatestId
    , deleteById
    , deleteAll) where

import           Lib
import           Data.Block
import           GHC.Generics
import           Database.PostgreSQL.Simple
import           Prelude hiding (id)
import qualified Data.Text as T

newtype Id = Id { getId :: Integer }
  deriving (Generic)

instance FromRow Id where


createConnection :: IO Connection
createConnection = connect
  $ defaultConnectInfo { connectUser = "bot"
                       , connectDatabase = "bot"
                       , connectPassword = "postgres"
                       }

addToDB :: Connection -> Block -> IO ()
addToDB con b = do
  execute
    con
    "INSERT INTO words VALUES (?, ?, ?, ?)"
    ((get1 . block) b, (get2 . block) b, (get3 . block) b, id b)
  return ()

deleteById :: Connection -> Integer -> IO ()
deleteById con id_ = do
  execute con "DELETE FROM words WHERE id = ?" [id_]
  return ()

deleteAll :: Connection -> IO ()
deleteAll con = do
  execute_ con "DELETE FROM words"
  return ()

readDB :: Connection -> IO [Block]
readDB con = toBlock <$> query_ con "SELECT * FROM words"

getLatestId :: Connection -> IO Integer
getLatestId con =
  getId . head <$> query_ con "SELECT MAX(id) FROM words" :: IO Integer

toBlock :: [(T.Text, T.Text, T.Text, Integer)] -> [Block]
toBlock [] = []
toBlock ((a, b, c, i):xs) = Block (a, b, c) i:toBlock xs