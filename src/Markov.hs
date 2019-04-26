module Markov where

import           Lib
import           Data.Block
import           Text.MeCab
import           Data.List.Split
import           Data.List

createMcb :: IO MeCab
createMcb = new2 ""

tokenize :: MeCab -> String -> IO [String]
tokenize mcb s = do
  ws <- map nodeSurface <$> parseToNodes mcb s
  return $ addSpace ws
  -- for english vocablary

 where
  addSpace :: [String] -> [String]
  addSpace []             = []
  addSpace [w           ] = [w]
  addSpace (w1 : w2 : ws) = if onlyAlp w1 w2
    then w1 : " " : addSpace (w2 : ws)
    else w1 : addSpace (w2 : ws)

  -- onlyAlp "Hello" "World" = True
  -- onlyAlp "Hello" "こんにちは" = False
  onlyAlp :: String -> String -> Bool
  onlyAlp w1 w2 = all (not . null) [w1, w2]
    && all (all (`elem` ['A' .. 'Z'] ++ ['a' .. 'z'])) [w1, w2]

createBlocks :: [String] -> Integer -> [Block]
createBlocks [f, s] _ = []
createBlocks (f : s : t : ws) i =
  createBlock (f, s, t) i : createBlocks (s : t : ws) i

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
createTweet = create 0
 where
  create c hs bs = do
    h   <- takeRandom hs
    bls <- connectBlocks h bs
    loop bls c hs bs

  loop bls c hs bs
    | isMatch bls = return $ Just (fmtToSentence bls, map getBId bls)
    | c < 100     = create (c + 1) hs bs
    | otherwise   = return Nothing

  isMatch bs =
    let len = length bs in len <= 10 && cost (map getBId bs) < len `div` 2

  cost = sum . map (flip (-) 1 . length) . group

  fmtToSentence xs =
    let b2s = map getW2 xs
        b3s = map getW3 xs
    in  foldl1 (++) $ zipWith (++) b2s b3s
