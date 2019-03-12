module Lib (rmvUnneeds, takeRdm) where

import           Data.List
import qualified Data.Text as T
import qualified System.Random as R

rmvUnneeds :: [T.Text] -> [T.Text]
rmvUnneeds [] = []
rmvUnneeds (tw:tws) = [tw | ok tw] ++ rmvUnneeds tws
  where
    ok :: T.Text -> Bool
    ok tw = all
      (\p -> not (T.isInfixOf (T.pack p) tw))
      ["https", "http", "RT @", "@", "#"]

takeRdm :: [a] -> IO a
takeRdm [] = error "Empty list"
takeRdm xs = do
  idx <- R.getStdRandom (R.randomR (0, length xs - 1))
  return $ xs !! idx
