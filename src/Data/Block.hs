module Data.Block (Block, createBlock, getW1, getW2, getW3, getWords, getBId)where

import           Lib
import           Prelude hiding (id, words)
import qualified Data.Text as T

data Block = MkBlock { words :: (T.Text, T.Text, T.Text), id :: Integer }
  deriving Show

instance Eq Block where
  (MkBlock (a, b, c) _) == (MkBlock (a', b', c') _) = a == a' && b == b' && c == c'

createBlock = MkBlock

getW1 = get1 . words

getW2 = get2 . words

getW3 = get3 . words

getWords = words

getBId = id