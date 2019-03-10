{-# LANGUAGE DeriveGeneric #-}
module Twitter
  ( getTweets
  , postTweet
  , rmvMine
  , fromTweet
  )
where

import           Config

import           Web.Authenticate.OAuth
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           GHC.Generics
import           Data.Aeson
import           Data.List
import           Data.Text.Encoding
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Internal as BL

newtype User = User {screen_name :: String} deriving (Show, Generic)
data Tweet = Tweet {text :: T.Text, user :: User} deriving (Show, Generic)

instance FromJSON Tweet
instance FromJSON User
instance ToJSON Tweet
instance ToJSON User

getName :: IO String
getName = name <$> getConfig

getKeys :: IO Keys
getKeys = keys <$> getConfig

getAuth = do
  keys <- getKeys
  return $ newOAuth { oauthServerName     = "api.twitter.com"
                    , oauthConsumerKey    = (B8.pack . ck) keys
                    , oauthConsumerSecret = (B8.pack . cs) keys
                    }
getCred = do
  keys <- getKeys
  return $ newCredential ((B8.pack . at) keys) ((B8.pack . as) keys)


getTweets :: IO [Tweet]
getTweets = do
  res <- do
    req <- parseRequest
      "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200"
    auth      <- getAuth
    cred      <- getCred
    signedReq <- signOAuth auth cred req
    man       <- newManager tlsManagerSettings
    httpLbs signedReq man
  return . either (error . show) id <$> eitherDecode $ responseBody res


postTweet :: T.Text -> IO Bool
postTweet s = do
  res <- do
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    man <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [(B8.pack "status", encodeUtf8 s)] req
    auth      <- getAuth
    cred      <- getCred
    signedReq <- signOAuth auth cred postReq
    httpLbs signedReq man
  return $ (statusCode . responseStatus) res == 200

rmvMine :: [Tweet] -> IO [Tweet]
rmvMine ts = do
  n <- getName
  return $ foldr (\tw -> (++) [ tw | (screen_name . user) tw /= n ]) [] ts

fromTweet :: Tweet -> T.Text
fromTweet = text

