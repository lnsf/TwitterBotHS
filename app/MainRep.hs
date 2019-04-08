module Main where

import           Lib
import           Markov
import           Twitter
import           Data.Block
import           Data.List
import           Data.Mention
import           Data.Tweet
import           Control.Monad
import           System.Exit
import           Prelude hiding (id)
import qualified Data.Text as T

main :: IO ()
main = do
  ms <- map fromMention <$> getMentions
  m <- createMcb
  forM_ ms $ \(i, u) -> do
    putStrLn u
    ts <- map fromTweet <$> getUserTweets u
    (hs, bs) <- do
      bls <- concat <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
      (return . partition isHead) bls
    tw <- fst <$> createTweet hs bs
    rep <- postReply i $ T.append (T.pack ("@" ++ u ++ " ")) tw
    unless rep exitFailure
    fav <- createFab i
    putStrLn $ T.unpack tw
    unless fav exitFailure
  where
    isHead b = getW1 b == T.empty

