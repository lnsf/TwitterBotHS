{-# LANGUAGE DeriveGeneric #-}

module Data.Tweet (Tweet(text, user), screen_name, tweetId, fromTweet) where

import           Prelude hiding (id)
import           Data.Aeson
import           GHC.Generics
import           Data.Text


newtype User = User { screen_name :: String }
  deriving (Show, Generic)

data Tweet = Tweet { text :: Text, user :: User, id :: Integer }
  deriving (Show, Generic)

instance FromJSON Tweet

instance FromJSON User

fromTweet :: Tweet -> (Text, Integer)
fromTweet t = (text t, id t)

tweetId = id