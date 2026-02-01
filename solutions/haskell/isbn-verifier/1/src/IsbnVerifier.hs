module IsbnVerifier (isbn) where
import Data.Maybe
import Data.List
import Data.Char (ord)

isbn2Digit c = 
  if c >= '0' && c <= '9' then Just (ord c - ord '0') 
  else if c == 'X' then Just 10 
  else Nothing

isbn2list = catMaybes . map isbn2Digit

isLastElement e [x] = e == x
isLastElement e (_:xs) = isLastElement e xs
isLastElement _ [] = True


dot x y = sum' $ zipWith (*) x y where sum' = foldl' (+) 0

isbn :: String -> Bool
isbn i = (length l == 10) && (b <= 1) && (s `mod` 11 == 0) where 
  l = isbn2list i
  b = length $ snd $ break (==10) l
  m = reverse $ take 10 [1..]
  s = dot l m
