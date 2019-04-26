{-# LANGUAGE DeriveGeneric #-}

module Data.Tweet
  ( Tweet(text, user)
  , User(..)
  , tweetId
  , fromTweet
  )
where

import           Prelude                 hiding ( id )
import           Data.Aeson
import           GHC.Generics

newtype User = User { screen_name :: String }
  deriving (Show, Generic)

data Tweet = Tweet { text :: String, user :: User, id :: Integer }
  deriving (Show, Generic)

instance FromJSON Tweet

instance FromJSON User

fromTweet :: Tweet -> (String, Integer)
fromTweet t = (text t, id t)

tweetId = id
