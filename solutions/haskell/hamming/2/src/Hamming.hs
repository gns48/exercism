module Hamming (distance) where

distance :: String -> String -> Maybe Int
hamming :: Int -> String -> String -> Maybe Int

distance xs ys = hamming 0 xs ys

hamming n [] [] = Just n
hamming n (x:xs) (y:ys) =
     if (x == y) then hamming n xs ys
     else hamming (n+1) xs ys
hamming _ _ _ = Nothing

