module Main (main) where

import System.Environment
import Data.List
import Data.Char
 
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, key, filepath] -> do
      content <- readFile filepath
      let result = case mode of
                     "e" -> encrypt key content
                     "d" -> decrypt key content
                     _   -> "Invalid mode"
      -- оба варианта записи результата               
      putStrLn result 
      writeFile "output.txt" result
    _ -> putStrLn "Usage: ./example <mode> <key> <filepath>"
                      
encrypt :: String -> String -> String
encrypt key = vigenere encryptChar key

decrypt :: String -> String -> String
decrypt key = vigenere decryptChar key

vigenere :: ((Char, Char) -> Char) -> String -> String -> String
vigenere f key str = zipWith (\x k -> if isLetterOrDigit x then f (toUpper x, toUpper k) else ' ') str (cycle key)

encryptChar, decryptChar :: (Char, Char) -> Char
encryptChar = shiftChar 1
decryptChar = shiftChar (-1)

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = isLetter c || isDigit c

shiftChar :: Int -> (Char, Char) -> Char
shiftChar multiplier (stringChar, keyChar)
      | isLetter stringChar = chr $ ((ord stringChar + multiplier * ord keyChar - 2 * ord 'A') `mod` 26) + ord 'A'
      | otherwise = ' '
