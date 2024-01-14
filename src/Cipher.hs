module Cipher(encrypt, decrypt) where

import Data.Char (chr, ord, isAsciiLower)
import Data.Function (on)

toNumber :: Char -> Int
toNumber = flip (-) 97 . ord

toChar :: Int -> Char
toChar = chr . (+ 97)

matchLength :: String -> String -> String
matchLength ks cs = matchLength' ks cs 0
  where
    matchLength' _ "" _ = ""
    matchLength' ks' (c':cs') index = if isAsciiLower c' 
      then ks' !! index : matchLength' ks' cs' ((index + 1) `mod` length ks')
      else c' : matchLength' ks' cs' index

encryptWith :: (Int -> Int -> Int) -> String -> String -> String
encryptWith f ks cs = zipWith encryptChar (matchLength ks cs) cs
  where
    encryptChar k c = if isAsciiLower c then toChar $ (on f toNumber k c) `mod` 26 else c

encrypt :: String -> String -> String
encrypt = encryptWith (+)

decrypt :: String -> String -> String
decrypt = encryptWith (flip (-)) 

