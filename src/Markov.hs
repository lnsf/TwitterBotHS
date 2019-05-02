module Markov where

import           Lib
import           Data.Block
import           Text.MeCab
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Control.Monad

createMecab :: IO MeCab
createMecab = new2 ""

tokenize :: MeCab -> String -> IO [String]
tokenize mcb s = map nodeSurface <$> parseToNodes mcb s

createBlocks :: [String] -> Integer -> [Block]
createBlocks ws i =
  let (_:w2:ws3) = ws
      ws2 = (w2:ws3)
  in zipWith3 (\a b c -> createBlock (a,b,c) i) ws ws2 ws3

connectBlocks :: Block -> [Block] -> IO [Block]
connectBlocks b bs = do
  next <- getNext b bs
  return $ b : next
 where
  connectable :: Block -> [Block] -> [Block]
  connectable b = filter (\x -> getW1 x == getW3 b)

  getNext :: Block -> [Block] -> IO [Block]
  getNext b bs = do
    let con = connectable b bs
    if null con
      then return []
      else do
        b2 <- takeRandom con
        b3 <- getNext b2 $ bs \\ [b2]
        return $ b2 : b3

createTweet :: [Block] -> [Block] -> IO (Maybe (String, [Integer]))
createTweet hs bs = foldM
  (\a _ -> if isNothing a then create hs bs else return a)
  Nothing
  [1 .. 100]
 where
  create hs bs = do
    h   <- takeRandom hs
    bls <- connectBlocks h bs
    if isMatch bls
      then return $ Just (fmtToSentence bls, map getBId bls)
      else return Nothing

  isMatch bs =
    let len = length bs in len <= 10 && cost (map getBId bs) < len `div` 2

  cost = sum . map (flip (-) 1 . length) . group

  fmtToSentence xs =
    let bs2 = map getW2 xs
        bs3 = map getW3 xs
    in  ((foldl1 (++) .) . zipWith (++)) bs2 bs3

separateHeadsBodies = partition ((==) "" . getW1)
