{-# LANGUAGE DeriveGeneric #-}
module Config
  ( getConfig
  , keys
  , ck
  , cs
  , at
  , as
  , Keys
  )
where

import           GHC.Generics
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B8
import qualified Data.Yaml                     as Y

newtype Config = Config{keys :: Keys} deriving (Show, Generic)
data Keys = Keys{ck :: String, cs :: String, at :: String, as :: String} deriving (Show, Generic)

instance Y.FromJSON Config
instance Y.FromJSON Keys

getConfig :: IO Config
getConfig = either (error . show) id <$> Y.decodeFileEither "./config.yaml"
