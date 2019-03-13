module Data.Block where

import           Prelude hiding (id)
import qualified Data.Text as T

data Block = Block { block :: (T.Text, T.Text, T.Text), id :: Integer }
  deriving Show

instance Eq Block where
  (Block (a, b, c) _) == (Block (a', b', c') _) = a == a && b == b && c == c

newBlock :: (T.Text, T.Text, T.Text) -> Integer -> Block
newBlock = Block