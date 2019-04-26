{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
  ( createConnection
  , withConnection
  , addToDB
  , readDB
  , getLatestId
  , deleteById
  , deleteAll
  )
where

import           Lib
import           Control.Exception
import           Data.Block
import           GHC.Generics
import           Database.PostgreSQL.Simple

newtype Id = Id { getId :: Integer }
  deriving (Generic)

instance FromRow Id where

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket createConnection close

createConnection :: IO Connection
createConnection = connect $ defaultConnectInfo { connectUser     = "bot"
                                                , connectDatabase = "bot"
                                                , connectPassword = "postgres"
                                                }

addToDB :: Connection -> Block -> IO ()
addToDB con b = do
  let (b1, b2, b3) = getWords b
      i            = getBId b
  execute con "INSERT INTO words VALUES (?, ?, ?, ?)" (b1, b2, b3, i)
  return ()

deleteById :: Connection -> Integer -> IO ()
deleteById con i = do
  execute con "DELETE FROM words WHERE id = ?" [i]
  return ()

deleteAll :: Connection -> IO ()
deleteAll con = do
  execute_ con "DELETE FROM words"
  return ()

readDB :: Connection -> IO [Block]
readDB con = map toBlock <$> query_ con "SELECT * FROM words"

getLatestId :: Connection -> IO Integer
getLatestId con =
  getId . head <$> query_ con "SELECT MAX(id) FROM words" :: IO Integer

toBlock :: (String, String, String, Integer) -> Block
toBlock (a, b, c, i) = createBlock (a, b, c) i
