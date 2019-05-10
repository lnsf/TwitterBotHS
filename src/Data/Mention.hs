{-# LANGUAGE DeriveGeneric #-}

module Data.Mention
    ( Mention
    , replyTo
    , isReply
    , mentionUser
    , mentionId
    , mentionText
    , favorited
    , fromMention) where

import           Data.Aeson
import           Data.Tweet   (User (..))
import           GHC.Generics
import           Prelude      hiding (id)

data Mention = Mention { id                      :: Integer
                       , text                    :: String
                       , in_reply_to_status_id   :: Maybe Integer
                       , in_reply_to_screen_name :: Maybe String
                       , user                    :: User
                       , favorited               :: Bool
                       }
  deriving (Show, Generic)

instance FromJSON Mention

replyTo = in_reply_to_screen_name

isReply = in_reply_to_status_id

mentionUser = user

mentionId = id

mentionText = text

fromMention :: Mention -> (Integer, String)
fromMention m = let i = id m
                    u = (screen_name . user) m
                in (i, u)
