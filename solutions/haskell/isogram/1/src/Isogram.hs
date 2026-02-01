module Isogram (isIsogram) where
import Data.Char as C
import Data.Maybe as M
import qualified Data.List as L

toLetter c = if z >= 'a' && z <= 'z' then Just z else Nothing
   where z = C.toLower c

checkLength l = if length l == 0 then True else L.maximum l == 1

isIsogram :: String -> Bool
isIsogram text = checkLength l where
  l = map (length) $ L.group $ L.sort $ M.catMaybes $ map toLetter text
