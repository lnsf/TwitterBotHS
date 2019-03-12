module Types (Block, DBEntry) where

import qualified Data.Text as T

type Block = (T.Text, T.Text, T.Text)

type DBEntry = (T.Text, T.Text, T.Text, Int)