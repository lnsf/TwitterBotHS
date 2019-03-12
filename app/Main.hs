module Main where

import           Types
import           Lib
import           Markov
import           Twitter
import           DB
import           Control.Monad
import           Prelude hiding (Word)
import qualified Data.Text as T

main :: IO ()
main = do
  ts <- map fromTweet <$> getTweets
  m <- createMcb
  bs <- forM ts
    $ \(t, i) -> do
      ws <- createBlocks <$> tokenize m t
      return $ map (`toDBEntry` i) ws
  c <- createConnection
  forM_ (concat bs) $ \b -> addToDB c b
  print =<< readDB c
-- ss <- forM (map head bs)
--   $ \h -> fromBlocks <$> connectBlocks h (concatMap tail bs)
-- twstr <- takeRdm [s | s <- ss, T.length s < 50]
-- res <- postTweet twstr
-- if res
--   then putStrLn (T.unpack twstr)
--   else putStrLn "Failed to tweet..."



