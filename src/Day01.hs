module Day01
    ( day01_0, day01_1
    ) where

import Control.Applicative
import Control.Monad
import Data.HashSet

deplus :: String -> String
deplus ('+':xs) = xs
deplus s = s

day01_0 :: IO ()
day01_0 = sum <$> liftM (read . deplus) <$> lines <$> readFile "day01.input" >>= print

find_repeat :: [(Int, Int)] -> Set Int -> Int -> Int
find_repeat ((x, freq):xs) set n 
    | member freq set = freq
    | otherwise = find_repeat xs (insert freq set) (n + 1)
     
frequences :: [Int] -> Int -> [(Int, Int)]
frequences (x:xs) freq = let new_freq = x + freq in (x, new_freq) : frequences xs new_freq

day01_1 :: IO ()
day01_1 = do
    input <- liftM (read . deplus) <$> lines <$> readFile "day01.input"
    let stream = frequences (cycle input) 0
    print $ find_repeat stream Data.HashSet.empty 0