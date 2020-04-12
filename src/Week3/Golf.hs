module Week3.Golf ( skips,
                    nthItems
) where

import Data.List

skips :: [a] -> [[a]]
skips l = snd $ mapAccumL (\x y -> (x + 1, nthItems x y)) 1 $ take (length l) (repeat l) 

nthItems :: Int -> [a] -> [a]
nthItems n l
    | n > 0 && n <= length l = (take 1 $ drop (n-1) l) ++ nthItems n (drop n l)
    | otherwise              = []
