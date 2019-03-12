module Markov
    ( createMcb
    , createBlocks
    , tokenize
    , connectBlocks
    , fromBlocks
    , Block) where

import           Lib (takeRdm)
import           Text.MeCab
import           Data.List.Split
import           Data.List
import qualified Data.Text as T

type Block = (T.Text, T.Text, T.Text)

createMcb :: IO MeCab
createMcb = new2 ""

tokenize :: MeCab -> T.Text -> IO [T.Text]
tokenize mcb s = do
  ws <- map nodeSurface <$> parseToNodes mcb s
  return $ addSpace ws
  -- for english vocablary

    where
      addSpace :: [T.Text] -> [T.Text]
      addSpace [] = []
      addSpace [w] = [w]
      addSpace (w1:w2:ws) = if onlyAlp w1 w2
                            then w1:T.pack " ":addSpace (w2:ws)
                            else w1:addSpace (w2:ws)

      onlyAlp :: T.Text -> T.Text -> Bool
      onlyAlp w1 w2 = not (T.null w1 || T.null w2)
        && all (T.all (`elem` ['A' .. 'z'])) [w1, w2]

createBlocks :: [T.Text] -> [Block]
createBlocks [f, s] = []
createBlocks (f:s:t:ws) = (f, s, t):createBlocks (s:t:ws)

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b:next
  where
    cancon b bs = [x | x <- bs, bfst x == blst b]

    getNext b bs = if null $ cancon b bs
                   then return []
                   else do
                     b2 <- takeRdm $ cancon b bs
                     b3 <- getNext b2 (bs \\ [b2])
                     return $ b2:b3

fromBlocks :: [Block] -> T.Text
fromBlocks = foldr (\b -> T.append (bfst b `T.append` bsnd b)) T.empty

bfst (f, _, _) = f

bsnd (_, s, _) = s

blst (_, _, l) = l
