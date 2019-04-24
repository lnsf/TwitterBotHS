module Markov where

import           Prelude hiding (id)
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
createBlocks (f:s:t:ws) i = createBlock (f, s, t) i:createBlocks (s:t:ws) i

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b:next
  where
    connectable :: Block -> [Block] -> [Block]
    connectable b = filter (\x -> getW1 x == getW3 b)

    getNext :: Block -> [Block] -> IO [Block]
    getNext b bs = do
      let
        con = connectable b bs
      if null con
        then return []
        else do
          b2 <- takeRandom con
          b3 <- getNext b2 (bs \\ [b2])
          return $ b2:b3

createTweet :: [Block] -> [Block] -> IO (T.Text, [Integer])
createTweet hs bs = do
  h <- takeRandom hs
  bls <- connectBlocks h bs
  if isMatch bls
    then return (fmtToSentence bls, map getBId bls)
    else createTweet hs bs
  where
    isMatch bs =
      let
        len = length bs
      in
        len <= 10 && cost (map getBId bs) < len `div` 2 

    cost = sum . map (flip (-) 1 . length) . group

    fmtToSentence xs =
      let
        b2s = map getW2 xs
        b3s = map getW3 xs
      in
        foldl1 T.append (zipWith T.append b2s b3s)
