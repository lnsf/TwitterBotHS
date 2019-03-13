module Markov where

import           Lib
import           Data.Block
import           Text.MeCab
import           Data.List.Split
import           Data.List
import qualified Data.Text as T

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

      -- onlyAlp "Hello" "World" = True
      -- onlyAlp "Hello" "こんにちは" = False
      onlyAlp :: T.Text -> T.Text -> Bool
      onlyAlp w1 w2 = not (T.null w1 || T.null w2)
        && all (T.all (`elem` ['A' .. 'Z'] ++ ['a' .. 'z'])) [w1, w2]

createBlocks :: [T.Text] -> Integer -> [Block]
createBlocks [f, s] _ = []
createBlocks (f:s:t:ws) i = newBlock (f, s, t) i:createBlocks (s:t:ws) i

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b:next
  where
    cancon :: Block -> [Block] -> [Block]
    cancon b bs = [x | x <- bs, (get1 . block) x == (get3 . block) b]

    getNext :: Block -> [Block] -> IO [Block]
    getNext b bs = if null $ cancon b bs
                   then return []
                   else do
                     b2 <- takeRdm $ cancon b bs
                     b3 <- getNext b2 (bs \\ [b2])
                     return $ b2:b3

fromBlocks :: [Block] -> (T.Text, [Integer])
fromBlocks xs =
  ( foldr
      (\b -> T.append ((get1 . block) b `T.append` (get2 . block) b))
      T.empty
      xs
  , map Data.Block.id xs)
