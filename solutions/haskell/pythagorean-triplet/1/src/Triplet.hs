module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = 
  [ (a,b,c) | 
    a <- [3..n], 
    b <- [a+1..n], 
    let c = n-a-b, 
    a*a + b*b == c*c]