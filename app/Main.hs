module Main where

import           Lib
import           Markov
import           Twitter
import           Control.Monad

import qualified Data.Text                     as T

main :: IO ()
main = do
  ts <- do
    t <- getTweets
    case t of
      Left  er  -> error "Can't get tweets"
      Right tws -> (return . rmvUnneeds . map fromTweet) tws

  bs <- forM ts $ \t -> do
    ws <- tokenize t
    (return . createBlocks) ws

  ss <- forM (map head bs)
    $ \h -> fromBlocks <$> connectBlocks h (concatMap tail bs)

  twstr <- takeRdm [ s | s <- ss, T.length s < 50 ]

  res   <- postTweet twstr
  if res then putStrLn (T.unpack twstr) else putStrLn "Failed to tweet..."



