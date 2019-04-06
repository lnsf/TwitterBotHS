module Twitter (getTweets, getTweetsAfter, postTweet) where

import           Config
import           Web.Authenticate.OAuth
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Data.Aeson
import           Data.Text.Encoding
import           Data.Tweet
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

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

getTweetsAfter :: Integer -> IO [Tweet]
getTweetsAfter minId = dropWhile (\t -> tweetId t <= minId) <$> getTweets

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
    Right ts -> ((removeByText .) . removeByName) <$> getName <*> pure ts

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

removeByName :: String -> [Tweet] -> [Tweet]
removeByName n = filter (\t -> (screen_name . user) t /= n)

removeByText :: [Tweet] -> [Tweet]
removeByText = filter (ok . text)
  where
    ok :: T.Text -> Bool
    ok tw = all
      (\p -> not (T.isInfixOf (T.pack p) tw))
      ["https", "http", "RT @", "@", "#"]
