import Lib
import Data.Bool

main :: IO ()
main = putStrLn $ bool "testSpecialChars not passed" "testSpecialChars passed" testSpecialChars

testSpecialChars :: Bool
testSpecialChars = encrypt "tajne" "to jest bardzo tajny tekst" == "mo srwm bjehso cnngy crolt"


