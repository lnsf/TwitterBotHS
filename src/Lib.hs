module Lib where

import           Data.List
import qualified Data.Text                     as T
import qualified System.Random                 as R

takeRandom :: [a] -> IO a
takeRandom [] = error "Empty list"
takeRandom xs = do
  idx <- R.getStdRandom (R.randomR (0, length xs - 1))
  return $ xs !! idx

get1 (a, _, _) = a

get2 (_, a, _) = a

get3 (_, _, a) = a
