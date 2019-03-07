module Markov
  ( createBlocks
  , tokenize
  , connectBlocks
  )
where

import           Text.MeCab
import           Data.List.Split
import           Data.List
import qualified Data.Text                     as T
import qualified System.Random                 as R

type Block = (T.Text, T.Text, T.Text)

tokenize :: T.Text -> IO [T.Text]
tokenize s = do
  mcb <- new2 ""
  map nodeSurface <$> parseToNodes mcb s

createBlocks :: [T.Text] -> [Block]
createBlocks [f, s]           = []
createBlocks (f : s : t : ws) = (f, s, t) : createBlocks (s : t : ws)

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b : next
 where
  bfst (f, _, _) = f
  blst (_, _, l) = l
  cancon b bs = [ x | x <- bs, bfst x == blst b ]
  getNext b bs = if null $ cancon b bs
    then return []
    else do
      b2 <- takeRdm $ cancon b bs
      b3 <- getNext b2 (bs \\ [b2])
      return $ b2 : b3

takeRdm :: [a] -> IO a
takeRdm [] = error "Empty list"
takeRdm xs = do
  idx <- R.getStdRandom (R.randomR (0, length xs - 1))
  return $ xs !! idx
