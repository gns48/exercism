module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates =  
   [next | next <- candidates, isAnagram word next]
 
isAnagram :: String -> String -> Bool
isAnagram word next =
    lw /= ln && sort lw == sort ln
  where
    lw = map toLower word
    ln = map toLower next

        
