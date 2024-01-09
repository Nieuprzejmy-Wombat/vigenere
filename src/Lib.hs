module Lib where

import Data.Char (chr, ord, isAsciiLower)

table :: [[Char]]
table = [shift n <$> ['a' .. 'z'] | n <- [0 .. 25]]
 where
  shift :: Int -> Char -> Char
  shift n c = chr $ (((ord c - 97) + n) `mod` 26) + 97

-- matchLength n cs repeats cs until its length is equal to n
matchLength :: Int -> String -> String
matchLength n cs = case compare n (length cs) of
  GT -> matchLength n $ cs <> cs
  EQ -> cs
  LT -> take n cs

encrypt :: String -> String -> String
encrypt ks cs = (<>)
  where
    encryptChar k c = if isAsciiLower c then table !! (ord k - 97) !! (ord c - 97) else c
    ks' = matchLength (length cs) ks


