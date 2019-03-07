module Markov
  ( createBlocks
  , tokenize
  , connectBlocks
  , toText
  , Block
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
  ws  <- map nodeSurface <$> parseToNodes mcb s
  return $ addSpace ws
 where
  -- for english vocablary
  addSpace :: [T.Text] -> [T.Text]
  addSpace []             = []
  addSpace [w           ] = [w]
  addSpace (w1 : w2 : ws) = if onlyAlp w1 w2
    then w1 : T.pack " " : addSpace (w2 : ws)
    else w1 : addSpace (w2 : ws)
  onlyAlp :: T.Text -> T.Text -> Bool
  onlyAlp w1 w2 =
    not (T.null w1 || T.null w2) && all (T.all (`elem` ['A' .. 'z'])) [w1, w2]


createBlocks :: [T.Text] -> [Block]
createBlocks [f, s]           = []
createBlocks (f : s : t : ws) = (f, s, t) : createBlocks (s : t : ws)

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b : next
 where
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

toText :: [Block] -> T.Text
toText = foldr (\b -> T.append (bfst b `T.append` bsnd b)) T.empty

bfst (f, _, _) = f
bsnd (_, s, _) = s
blst (_, _, l) = l
