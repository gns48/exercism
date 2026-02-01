module SumOfMultiples (sumOfMultiples) where

import Data.List

mult :: Integer -> Integer -> [Integer]
mult factor limit = 
   if factor == 0 then [0]
   else takeWhile (< limit) $ map (*factor) [1..]

mapFactors :: [Integer] -> [Integer] -> Integer -> [Integer] 
mapFactors rs [] _ = rs
mapFactors rs (f:fs) limit = mapFactors (rs ++ (mult f limit)) fs limit

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = foldr (+) 0 result where
   result = nub $ sort $ mapFactors [] factors limit
