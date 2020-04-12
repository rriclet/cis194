module Week3.Golf ( skips,
                    nthItems
) where

import Data.List

skips :: [a] -> [[a]]
skips l = snd $ mapAccumL (\x y -> (x + 1, nthItems x y)) 1 $ take (length l) (repeat l) 

nthItems :: Int -> [a] -> [a]
nthItems n l = if n > 0 && n <= length l 
               then (take 1 $ drop (n-1) l) ++ nthItems n (drop n l)
               else []
