module Main where

import           DB
import           Lib
import           Markov
import           Twitter
import           Data.Block
import           Data.List
import           Control.Monad
import           Prelude hiding (id)
import qualified Data.Text as T

main :: IO ()
main = do
  ts <- map fromTweet <$> getTweets :: IO [(T.Text, Integer)]
  m <- createMcb
  bs <- forM ts (\(t, i) -> flip createBlocks i <$> tokenize m t)
    :: IO [[Block]]
  c <- createConnection
  forM_ (concat bs) $ \b -> addToDB c b
  print =<< readDB c
-- ss <- forM
--   (map head bs)
--   (\h -> fromBlocks <$> connectBlocks h (concatMap tail bs))
--   :: IO [(T.Text, [Integer])]
-- twstr <- takeRdm
--   [fst s
--   | s <- ss
--   , (T.length . fst) s < 50               -- get texts them length are < 50
--   , (length . group . sort . snd) s > 3]  -- ignore biased tweets
--   :: IO T.Text
-- res <- postTweet twstr :: IO Bool
-- if res
--   then putStrLn (T.unpack twstr)
--   else putStrLn "Failed to tweet..."
