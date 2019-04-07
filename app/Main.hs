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
import qualified Data.Text as T

main :: IO ()
main = doTask =<< getArgs
  where
    usage = do
      pname <- getProgName
      putStrLn $ "Usage: " ++ pname ++ " [add|clean|tweet|help]"

    doTask [] = usage >> exitFailure
    doTask (arg:_)
      | arg `isPrefixOf` "add" = add
      | arg `isPrefixOf` "clean" = clean
      | arg `isPrefixOf` "tweet" = tweet
      | arg `isPrefixOf` "help" = usage >> exitSuccess
      | otherwise = usage >> exitFailure

add :: IO ()
add = withConnection $ \c -> do
  minId <- do
    xs <- readDB c
    if null xs
      then return 0
      else getLatestId c
  ts <- filter (\(_, i) -> i > minId) . map fromTweet <$> getTweets
    :: IO [(T.Text, Integer)]
  m <- createMcb
  bs <- concat <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
    :: IO [Block]
  mapM_ (addToDB c) bs
  putStrLn $ "Added " ++ (show . length) bs ++ " Blocks"

tweet :: IO ()
tweet = withConnection $ \c -> do
  (hs, bs) <- do
    xs <- readDB c
    if null xs
      then error "No data in Database"
      else (return . partition isHead) xs :: IO ([Block], [Block])
  tw <- createTweet hs bs :: IO (T.Text, [Integer])
  res <- (postTweet . fst) tw :: IO Bool
  if res
    then putStrLn $ (T.unpack . fst) tw
    else return $ error "Failed to tweet"
  forM_ (snd tw) $ \id -> deleteById c id
  where
    isHead b = getW1 b == T.empty

clean :: IO ()
clean = withConnection deleteAll

createTweet hs bs = do
  h <- takeRandom hs
  tw <- fromBlocks <$> connectBlocks h bs
  if isMatch tw
    then return tw
    else createTweet hs bs
  where
    isMatch (str, ids) = T.length str <= 30 && cost ids < length ids `div` 2

    cost :: [Integer] -> Int
    cost = sum . map (flip (-) 1 . length) . group