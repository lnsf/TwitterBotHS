{-# LANGUAGE BlockArguments #-}

module Main where

import           DB
import           Lib
import           Markov
import           Twitter
import           Data.Block
import           Data.List
import           Data.Mention
import           Data.Tweet
import           Control.Monad
import           System.Environment
import           System.Exit
import           Prelude hiding (id)
import qualified Data.Text as T

main :: IO ()
main = do
  ms <- map fromMention <$> getMentions
  m <- createMcb
  forM_ ms $ \(i, _, u, _) -> do
    putStrLn u
    ts <- map fromTweet <$> getUserTweets u
    (hs, bs) <- do
      bls <- concat <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
      (return . partition isHead) bls
    tw <- fst <$> createTweet hs bs
    rep <- postReply i $ T.append (T.pack ("@" ++ u ++ " ")) tw
    fav <- if not rep then exitFailure else createFab i
    putStrLn (T.unpack tw)
    if not fav then exitFailure else return ()
  where
    isHead b = (get1 . block) b == T.empty

    createTweet hs bs = do
      h <- takeRdm hs
      tw <- fromBlocks <$> connectBlocks h (bs \\ [h])
      if isMatch tw
        then return tw
        else createTweet hs bs
      where
        isMatch (str, ids) = T.length str <= 30 && cost ids < length ids `div` 2
    
        cost :: [Integer] -> Int
        cost = sum . map (flip (-) 1 . length) . group

