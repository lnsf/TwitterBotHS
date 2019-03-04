module Main where

import           Lib

import qualified Text.MeCab                    as M

main :: IO ()
main = do
  mcb <- M.new2 ""
  s   <- M.parse mcb "今日もいい天気"

  putStrLn s
