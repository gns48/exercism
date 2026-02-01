module PrimeFactors (primeFactors) where
import Data.List

primeFactors 1 = []
primeFactors n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ primeFactors $ div n prime
