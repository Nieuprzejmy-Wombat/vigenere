module Main (main) where

import Cipher (encrypt, decrypt)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  action <- getAction
  keyword <- putStr "keyword: " >> hFlush stdout >> getLine
  msg <- putStr "text to encrypt/decrypt: " >> hFlush stdout >> getLine
  putStrLn $ action keyword msg 

getAction :: IO (String -> String -> String)
getAction = do
  action <- putStr "action (encrypt/decrypt): " >> hFlush stdout >> getLine
  case action of
    "encrypt" -> return encrypt
    "decrypt" -> return decrypt
    _ -> putStrLn "incorrect input" >> hFlush stdout >> getAction


