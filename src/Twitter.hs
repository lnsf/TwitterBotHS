{-# LANGUAGE DeriveGeneric #-}
module Twitter
  ( getTweets
  , postTweet
  , rmvMine
  , fromTweet
  )
where

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

userName = "name"

newtype User = User {screen_name :: String} deriving (Show, Generic)
data Tweet = Tweet {text :: T.Text, user :: User} deriving (Show, Generic)

instance FromJSON Tweet
instance FromJSON User
instance ToJSON Tweet
instance ToJSON User

auth = newOAuth { oauthServerName     = "api.twitter.com"
                , oauthConsumerKey    = B8.pack "ck"
                , oauthConsumerSecret = B8.pack "cs"
                }
cred = newCredential (B8.pack "at") (B8.pack "as")


getTweets :: IO (Either String [Tweet])
getTweets = do
  res <- do
    req <- parseRequest
      "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200"
    signedReq <- signOAuth auth cred req
    man       <- newManager tlsManagerSettings
    httpLbs signedReq man
  return $ eitherDecode $ responseBody res

postTweet :: T.Text -> IO Bool
postTweet s = do
  res <- do
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    man <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [(B8.pack "status", encodeUtf8 s)] req
    signedReq <- signOAuth auth cred postReq
    httpLbs signedReq man
  return $ (statusCode . responseStatus) res == 200

rmvMine :: [Tweet] -> [Tweet]
rmvMine = foldr (\tw -> (++) [ tw | (screen_name . user) tw /= userName ]) []

fromTweet :: Tweet -> T.Text
fromTweet = text

