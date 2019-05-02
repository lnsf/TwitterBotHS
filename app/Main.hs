module Main where

import           DB
import           Lib
import           Markov
import           Twitter
import           Data.Block
import           Data.List
import           Data.Tweet
import           Control.Monad
import           System.Environment
import           System.Exit

main :: IO ()
main = doTask =<< getArgs
 where
  usage = do
    pname <- getProgName
    putStrLn $ "Usage: " ++ pname ++ " [add|clean|tweet|help]"

  doTask [] = usage *> exitFailure
  doTask (arg : _) | arg `isPrefixOf` "add"   = add
                   | arg `isPrefixOf` "clean" = clean
                   | arg `isPrefixOf` "tweet" = tweet
                   | arg `isPrefixOf` "help"  = usage *> exitSuccess
                   | otherwise                = usage *> exitFailure

add :: IO ()
add = withConnection $ \c -> do
  minId <- do
    xs <- readDB c
    if null xs then return 0 else getLatestId c
  ts <- filter ((<=) minId . snd) . map fromTweet <$> getTweets
  m  <- createMecab
  bs <- concat <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
  mapM_ (addToDB c) bs
  putStrLn $ "Added " ++ (show . length) bs ++ " Blocks"

tweet :: IO ()
tweet = withConnection $ \c -> do
  (hs, bs) <- do
    xs <- readDB c
    if null xs
      then error "No data in Database"
      else (return . separateHeadsBodies) xs
  maybetw <- createTweet hs bs
  case maybetw of
    Just tw -> do
      res <- (postTweet . fst) tw
      if res
        then putStrLn $ fst tw ++ " " ++ (show . length . snd) tw
        else return $ error "Failed to tweet"
      forM_ (snd tw) (deleteById c)
    Nothing -> clean *> add *> tweet

clean :: IO ()
clean = withConnection deleteAll
