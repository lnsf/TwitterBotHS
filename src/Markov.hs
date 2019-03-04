module Markov
  ( createBlocks
  )
where

import           Text.MeCab
import           Data.List.Split

createBlocks :: String -> IO [String]
createBlocks s = do
  mcb <- new2 ""
  map nodeSurface <$> parseToNodes mcb s
