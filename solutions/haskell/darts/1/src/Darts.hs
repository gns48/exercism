module Darts (score) where

score :: Float -> Float -> Int
score x y
  | dist <= 1.0 = 10
  | dist <= 5.0 = 5
  | dist <= 10.0 = 1
  | otherwise = 0
  where dist = sqrt ((x*x) + (y*y))
