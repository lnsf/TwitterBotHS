module Markov
  ( createBlocks
  , tokenize
  )
where

import           Text.MeCab
import           Data.List.Split
import qualified Data.Text                     as T

type Block = (T.Text, T.Text, T.Text)

tokenize :: T.Text -> IO [T.Text]
tokenize s = do
  mcb <- new2 ""
  map nodeSurface <$> parseToNodes mcb s

createBlocks :: [T.Text] -> [Block]
createBlocks [f, s]           = []
createBlocks (f : s : t : ws) = (f, s, t) : createBlocks (s : t : ws)

