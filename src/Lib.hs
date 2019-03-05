module Lib
  ( rmvUnneeds
  )
where

import qualified Data.List                     as L
import qualified Data.Text                     as T

rmvUnneeds :: [T.Text] -> [T.Text]
rmvUnneeds []         = []
rmvUnneeds (tw : tws) = [ tw | ok tw ] ++ rmvUnneeds tws
 where
  ok :: T.Text -> Bool
  ok tw =
    L.all (\p -> not (T.isInfixOf (T.pack p) tw)) ["http", "RT @", "@", "#"]
