module Day11
    ( day11_0, day11_1
    ) where

import Data.Array 
import Data.Ord (comparing)
import Data.List (maximumBy)
import Control.Parallel.Strategies

serialNumber = 9798

power :: (Int, Int) -> Int
power (x, y) = 
    let 
        rackID = fromIntegral (x + 10) :: Float        
        n = (rackID * fromIntegral y + serialNumber) * rackID
    in
        if n >= 100 then (floor (n / 100.0)) `mod` 10 - 5 else 0

grid :: Array (Int, Int) Int
grid = listArray ((0, 0), (300, 300)) [power (x, y) | x <- [0..300], y <- [0..300]]

square3x3 :: Array (Int, Int) Int -> (Int, Int) -> Int
square3x3 arr (x, y) = sum [grid ! (x + dx, y + dy) | dx <- [0..2], dy <- [0..2]]

day11_0 :: IO ()
day11_0 = print $ maximumBy (comparing snd) [((x, y), square3x3 grid (x, y)) | x <- [0 .. 300 - 3], y <- [0..300 - 3]]
        
squareAny :: Array (Int, Int) Int -> (Int, Int) -> Int -> ((Int, Int, Int), Int)
squareAny arr (x, y) l = ((x, y, l + 1), sum [grid ! (x + dx, y + dy) | dx <- [0..l], dy <- [0..l]])

parallelSolve :: ((Int, Int, Int), Int)
parallelSolve = runEval $ do 
    let ranges = [[0..49], [50..99], [99..199], [200..299]]
    let tasks = let squares r = concat [squareAny grid (x, y) <$> [1 .. min (299 - max x y) 299] | x <- r, y <- [0..299]]
                in maximumBy (comparing snd) . squares <$> ranges
    maximums <- sequence $ rpar <$> tasks    
    return $ maximumBy (comparing snd) maximums

day11_1 :: IO ()
day11_1 = print $ parallelSolve


    