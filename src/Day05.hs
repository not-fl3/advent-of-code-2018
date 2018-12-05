module Day05
    ( day05_0, day05_1
    ) where

import Data.Char

reacts :: Char -> Char -> Bool 
reacts x y | x == y = False
           | otherwise = toUpper x == y || toUpper y == x

step :: String -> Int -> (Int, String)
step (a:x:y:xs) n | reacts x y = step (a:xs) (n + 1)
step (x:y:xs) n | reacts x y = step xs (n + 1)
step (x:xs) n = let (n', reduced) = step xs n in  (n', x:reduced)                
step s n = (n, s)

reduce :: String -> String
reduce s = let (n, reduced) = step s 0 in if n == 0 then reduced
                                                    else reduce reduced
day05_0 :: IO ()
day05_0 = readFile "day05.input1" >>= print . length . reduce . filter isLetter
    
remove :: String -> Char -> String 
remove s x = filter (\t -> t /= x && t /= toUpper x) s

day05_1 :: IO ()
day05_1 = do 
    input <- readFile "day05.input"
    print $ minimum $ length . reduce . filter isLetter <$> remove input <$> ['a'..'z']    
    
    