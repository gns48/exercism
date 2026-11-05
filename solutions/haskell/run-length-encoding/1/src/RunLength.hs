module RunLength (decode, encode) where

import Data.List (group) 
import Data.Char

decoder :: Int -> String -> String
decoder _ [] = []
decoder repCount (x:xs) = 
  if isDigit x then decoder (repCount * 10 + digitToInt x) xs
  else if repCount == 0 then x : decode xs
  else (replicate repCount x) ++ decode xs

decode :: String -> String
decode encodedText = decoder 0 encodedText

encoder :: String -> String
encoder s = if l > 1 then (show l) ++ [head s] else s 
  where l = length s

encode :: String -> String
encode s = concatMap encoder $ group s
  