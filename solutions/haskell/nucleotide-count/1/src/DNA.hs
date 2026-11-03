module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

readNucleotide :: Char -> Either String Nucleotide
readNucleotide c
  | c == 'A' = Right A
  | c == 'C' = Right C
  | c == 'G' = Right G
  | c == 'T' = Right T
  | otherwise = Left "error"
        
nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts = foldM updateMap acc
  where 
    updateMap m c = (flip . M.adjust) (+ 1) m <$> readNucleotide c
    acc = M.fromList [(A,0),(C,0),(G,0),(T,0)]
    