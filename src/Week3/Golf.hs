module Week3.Golf ( skips,
                    nthItems,
                    localMaxima,
                    histogram,
                    plotLine
) where

import Data.List

skips :: [a] -> [[a]]
skips l = snd $ mapAccumL (\x y -> (x + 1, nthItems x y)) 1 $ take (length l) (repeat l) 

nthItems :: Int -> [a] -> [a]
nthItems n l
    | n > 0 && n <= length l = (take 1 $ drop (n-1) l) ++ nthItems n (drop n l)
    | otherwise              = []

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3 = x2 : localMaxima ([x3] ++ xs)
    | otherwise          =      localMaxima ([x2, x3] ++ xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram l = (concatMap (plotLine 10) $ reverse $ transpose $ group $ sort $ map fromIntegral l) ++ "=========\n0123456789\n"

plotLine :: Int -> [Int] -> String
plotLine _ []     = "\n"
plotLine size (x:xs)
    | xs == []  = fill x ++ "*" ++ fill sizeLeft ++ "\n"
    | otherwise = fill x ++ "*" ++ plotLine sizeLeft (map (subtract (x+1)) xs)
    where sizeLeft = size - x - 1

fill :: Int -> String
fill x = concat $ replicate x " "