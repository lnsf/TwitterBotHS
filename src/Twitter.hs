{-# LANGUAGE DeriveGeneric #-}

module Twitter (getTweets, postTweet, fromTweet) where

import           Config
import           Web.Authenticate.OAuth
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           GHC.Generics
import           Data.Aeson
import           Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

type Name = String

newtype User = User { screen_name :: String }
  deriving (Show, Generic)

data Tweet = Tweet { text :: T.Text, user :: User, id :: Integer }
  deriving (Show, Generic)

instance FromJSON Tweet

instance FromJSON User

instance ToJSON User

getName :: IO String
getName = name <$> getConfig

getKeys :: IO Keys
getKeys = keys <$> getConfig

getAuth = do
  keys <- getKeys
  return
    $ newOAuth { oauthServerName = "api.twitter.com"
               , oauthConsumerKey = (B8.pack . ck) keys
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
    auth <- getAuth
    cred <- getCred
    signedReq <- signOAuth auth cred req
    man <- newManager tlsManagerSettings
    httpLbs signedReq man
  let dc = eitherDecode $ responseBody res
  case dc of
    Left er  -> error er
    Right ts -> flip rmvUnneeds ts <$> getName

postTweet :: T.Text -> IO Bool
postTweet s = do
  res <- do
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    man <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [(B8.pack "status", encodeUtf8 s)] req
    auth <- getAuth
    cred <- getCred
    signedReq <- signOAuth auth cred postReq
    httpLbs signedReq man
  return $ (statusCode . responseStatus) res == 200

rmvUnneeds :: Name -> [Tweet] -> [Tweet]
rmvUnneeds n = byName n . byText
  where
    byName :: Name -> [Tweet] -> [Tweet]
    byName n = foldr (\tw -> (++) [tw | (screen_name . user) tw /= n]) []

    byText :: [Tweet] -> [Tweet]
    byText [] = []
    byText (tw:tws) = [tw | (ok . text) tw] ++ byText tws
      where
        ok :: T.Text -> Bool
        ok tw = all
          (\p -> not (T.isInfixOf (T.pack p) tw))
          ["https", "http", "RT @", "@", "#"]

fromTweet :: Tweet -> (T.Text, Integer)
fromTweet t = (text t, Twitter.id t)

