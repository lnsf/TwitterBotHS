module Main where

import           DB
import           Lib
import           Markov
import           Twitter
import           Data.Block
import           Data.List
import           Control.Monad
import           System.Environment
import           System.Exit
import           Prelude hiding (id)
import qualified Data.Text as T

main :: IO ()
main = doTask =<< getArgs
  where
    usage = do
      pname <- getProgName
      putStrLn $ "Usage: " ++ pname ++ " [add|tweet|help]"

    doTask [] = usage >> exitFailure
    doTask (arg:_)
      | arg `isPrefixOf` "add" = add
      | arg `isPrefixOf` "tweet" = tweet
      | arg `isPrefixOf` "help" = usage >> exitSuccess
      | otherwise = usage >> exitFailure

add :: IO ()
add = do
  c <- createConnection
  minId <- do
    xs <- readDB c
    if null xs
      then return 0
      else getLatestId c
  ts <- filter (\(_, id) -> id > minId) . map fromTweet <$> getTweets
    :: IO [(T.Text, Integer)]
  m <- createMcb
  bs <- concat <$> mapM (\(t, i) -> flip createBlocks i <$> tokenize m t) ts
    :: IO [Block]
  mapM_ (addToDB c) bs
  putStrLn $ "Added " ++ (show . length) bs ++ " Blocks"

tweet :: IO ()
tweet = do
  c <- createConnection
  (hs, bs) <- do
    xs <- readDB c
    if null xs
      then error "No data in Database"
      else return (takeHeads xs, xs \\ takeHeads xs) :: IO ([Block], [Block])
  tw <- createTweet hs bs :: IO (T.Text, [Integer])
  res <- (postTweet . fst) tw :: IO Bool
  if res
    then putStrLn $ (T.unpack . fst) tw
    else return $ error "Failed to tweet"
  forM_ (snd tw) $ \id -> deleteById c id
  where
    takeHeads = filter (\b -> (get1 . block) b == T.empty)

createTweet hs bs = do
  h <- takeRdm hs
  tw <- fromBlocks <$> connectBlocks h (bs \\ [h])
  if isMatch tw
    then return tw
    else createTweet hs bs
  where
    isMatch (str, ids) = T.length str <= 50
      && (maximum . map length . group) ids < length ids `div` 2