{-# LANGUAGE DeriveGeneric #-}

module Config where

import           GHC.Generics
import           System.Environment
import qualified Data.Yaml as Y
import Data.List

data Config = Config { name :: String, keys :: Keys }
  deriving (Show, Generic)

data Keys = Keys { ck :: String, cs :: String, at :: String, as :: String }
  deriving (Show, Generic)

instance Y.FromJSON Config

instance Y.FromJSON Keys

getConfig :: IO Config
getConfig = do
  p <- dropWhileEnd (/= '/') <$> getExecutablePath
  either (error . show) id <$> Y.decodeFileEither (p ++ "config.yaml")
