module House (rhyme) where

import Data.List (inits)

thisis :: [String]
thisis = map (\s -> "This is the " ++ s ++ "\n")
  ["house that Jack built.",
    "malt",
    "rat",
    "cat",
    "dog",
    "cow with the crumpled horn",
    "maiden all forlorn",
    "man all tattered and torn",
    "priest all shaven and shorn",
    "rooster that crowed in the morn",
    "farmer sowing his corn",
    "horse and the hound and the horn"]

thats :: [String]
thats = map (\s -> "that " ++ s ++ "\n")
  ["lay in the house that Jack built.",
    "ate the malt",
    "killed the rat",
    "worried the cat",
    "tossed the dog",
    "milked the cow with the crumpled horn",
    "kissed the maiden all forlorn",
    "married the man all tattered and torn",
    "woke the priest all shaven and shorn",
    "kept the rooster that crowed in the morn",
    "belonged to the farmer sowing his corn"]

comb :: [String] -> [[String]] -> [String]
comb [] [] = []
comb (ts:tss) (th:thh) = ( ts ++ (concat $ reverse th) ++ "\n") : comb tss thh

rhyme = init $ concat $ comb thisis $ inits thats

