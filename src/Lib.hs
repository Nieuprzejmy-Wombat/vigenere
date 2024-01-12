module Lib where

import Data.Char (chr, ord, isAsciiLower)

table :: [[Char]]
table = [shift n <$> ['a' .. 'z'] | n <- [0 .. 25]]
 where
  shift :: Int -> Char -> Char
  shift n c = chr $ (((ord c - 97) + n) `mod` 26) + 97

-- make key equal in length to message by repeating it and copying special characters
matchLength :: String -> String -> String
matchLength ks cs = matchLength' ks cs 0
  where
    matchLength' ks' cs' index
      | null cs' = ""
      | not . isAsciiLower . head $ cs' = head cs' : matchLength' ks' (tail cs') index
      | otherwise = ks' !! index : matchLength' ks' (tail cs') (if index + 1 == length ks' then 0 else index + 1) 


encrypt :: String -> String -> String
encrypt ks cs = zipWith encryptChar (matchLength ks cs) cs
  where
    encryptChar k c = if isAsciiLower c then table !! (ord k - 97) !! (ord c - 97) else c


