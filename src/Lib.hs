module Lib (table) where

import Data.Char (chr, ord)

-- 97 - 122
table :: [[Char]]
table = [shift n <$> ['a' .. 'z'] | n <- [0 .. 25]]
 where
  shift :: Int -> Char -> Char
  shift n c = chr $ (((ord c - 97) + n) `mod` 26) + 97
