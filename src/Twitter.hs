{-# LANGUAGE DeriveGeneric #-}
module Twitter
  ( getTweets
  , postTweet
  )
where

import           Web.Authenticate.OAuth
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           GHC.Generics
import           Data.Aeson
import           Data.Text
import           Data.Text.Encoding
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Internal as BL

userName = "user name"

newtype Tweet = Tweet {text :: Text} deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

auth = newOAuth { oauthServerName     = "api.twitter.com"
                , oauthConsumerKey    = B8.pack "consumer key"
                , oauthConsumerSecret = B8.pack "consumer key secret"
                }
cred = newCredential (B8.pack "access token") (B8.pack "access token secret")


getTweets :: IO (Either String [Tweet])
getTweets = do
  res <- do
    req <- parseRequest
      "https://api.twitter.com/1.1/statuses/home_timeline.json"
    signedReq <- signOAuth auth cred req
    man       <- newManager tlsManagerSettings
    httpLbs signedReq man
  return $ eitherDecode $ responseBody res

postTweet :: Text -> IO Bool
postTweet s = do
  res <- do
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    man <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [(B8.pack "status", encodeUtf8 s)] req
    signedReq <- signOAuth auth cred postReq
    httpLbs signedReq man
  return $ (statusCode . responseStatus) res == 200



