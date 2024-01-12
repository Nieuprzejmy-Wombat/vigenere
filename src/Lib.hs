module Lib where

import Data.Char (chr, ord, isAsciiLower)

table :: [[Char]]
table = [shift n <$> ['a' .. 'z'] | n <- [0 .. 25]]
 where
  shift :: Int -> Char -> Char
  shift n = chr . (+ 97) . (`mod` 26) . (+ n) . (flip (-) 97) . ord

matchLength :: String -> String -> String
matchLength ks cs = matchLength' ks cs 0
  where
    matchLength' _ "" _ = ""
    matchLength' ks' (c':cs') index = if isAsciiLower c' 
      then ks' !! index : matchLength' ks' cs' ((index + 1) `mod` length ks')
      else c' : matchLength' ks' cs' index

encrypt :: String -> String -> String
encrypt ks cs = zipWith encryptChar (matchLength ks cs) cs
  where
    encryptChar k c = if isAsciiLower c then table !! (ord k - 97) !! (ord c - 97) else c

decrypt :: String -> String -> String
decrypt ks cs = encrypt ks' cs
  where ks' = chr . (+ 97) . (`mod` 26) . (26 -) . (flip (-) 97) . ord <$> ks

