{-# LANGUAGE DeriveGeneric #-}

module Config where

import           GHC.Generics
import qualified Data.Yaml as Y

data Config = Config { name :: String, keys :: Keys }
  deriving (Show, Generic)

data Keys = Keys { ck :: String, cs :: String, at :: String, as :: String }
  deriving (Show, Generic)

instance Y.FromJSON Config

instance Y.FromJSON Keys

getConfig :: IO Config
getConfig = either (error . show) id <$> Y.decodeFileEither "./config.yaml"
