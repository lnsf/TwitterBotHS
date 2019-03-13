module Lib where

import           Data.List
import qualified Data.Text as T
import qualified System.Random as R

takeRdm :: [a] -> IO a
takeRdm [] = error "Empty list"
takeRdm xs = do
  idx <- R.getStdRandom (R.randomR (0, length xs - 1))
  return $ xs !! idx

get1 (a, _, _) = a

get2 (_, a, _) = a

get3 (_, _, a) = a
