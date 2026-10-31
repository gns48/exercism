module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse rnaChar
  where
    rnaChar 'G' = Right 'C'
    rnaChar 'C' = Right 'G'
    rnaChar 'T' = Right 'A'
    rnaChar 'A' = Right 'U'
    rnaChar c   = Left c