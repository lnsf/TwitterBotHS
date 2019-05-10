module Main where

import           Bot.Markov
import           Bot.Twitter
import           Control.Monad
import           Data.Block
import           Data.List
import           Data.Mention
import qualified Data.Text     as T
import           Data.Tweet
import           Lib
import           System.Exit

main :: IO ()
main = do
  ms <- map fromMention <$> getMentions
  m <- createMecab
  forM_ ms
    $ \(i, u) -> do
      putStrLn u
      ts <- map fromTweet <$> getUserTweets u
      (hs, bs) <- do
        bls <- concat
          <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
        (return . separateHeadsBodies) bls
      tw <- fst
        <$> do
          maybetw <- createTweet hs bs
          case maybetw of
            Just jtw -> return jtw
            Nothing  -> error "Failed to create"
      rep <- postReply i $ "@" ++ u ++ " " ++ tw
      unless rep exitFailure
      fav <- createFab i
      putStrLn tw
      unless fav exitFailure
