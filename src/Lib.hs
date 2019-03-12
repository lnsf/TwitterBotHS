module Lib (takeRdm, toDBEntry) where

import           Types
import           Data.List
import           Prelude hiding (Word)
import qualified Data.Text as T
import qualified System.Random as R

takeRdm :: [a] -> IO a
takeRdm [] = error "Empty list"
takeRdm xs = do
  idx <- R.getStdRandom (R.randomR (0, length xs - 1))
  return $ xs !! idx

toDBEntry :: Block -> Int -> DBEntry
toDBEntry (a, b, c) i = (a, b, c, i)