{-# LANGUAGE DeriveGeneric #-}

module Data.Mention (Mention, replyTo, isReply, mentionUser, mentionId, mentionText, favorited, fromMention) where

import           Prelude hiding (id)
import           Data.Aeson
import           Data.Tweet (User(..))
import           GHC.Generics
import qualified Data.Text as T

data Mention = Mention { id :: Integer
                       , text :: T.Text
                       , in_reply_to_status_id :: Maybe Integer
                       , in_reply_to_screen_name :: Maybe String
                       , user :: User
                       , favorited :: Bool
                       }
                       deriving (Show, Generic)

instance FromJSON Mention

replyTo = in_reply_to_screen_name

isReply = in_reply_to_status_id

mentionUser = user

mentionId = id

mentionText = text

fromMention :: Mention -> (Integer, Maybe String, String, Bool)
fromMention m = 
  let
    i = id m
    r = in_reply_to_screen_name m
    u = (screen_name . user) m
    f = favorited m
  in
    (i, r, u, f)






