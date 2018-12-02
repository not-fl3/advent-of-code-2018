module Day02
    ( day02_0, day02_1
    ) where

import Control.Applicative
import Control.Monad
import Data.HashMap as HM
import Data.HashSet as HS (toList, fromList)
import Prelude

count :: String -> [Int]
count s = 
    let
        occurancies (x:xs) map = occurancies xs $ HM.insertWith (+) x 1 map
        occurancies "" map = map
    in 
        HS.toList . HS.fromList $ HM.elems $ HM.filter (> 1) $ occurancies s HM.empty

day02_0 :: IO ()
day02_0 = do
    x <- liftM count <$> lines <$> readFile "day02.input"
    print $ product . elems $ foldr (flip $ foldr f) HM.empty x
        where 
            f x = HM.insertWith (+) x 1

correct :: String -> String -> Maybe String
correct s1 s2 = f ([], s1) ([], s2)
    where 
        f (_, []) _ = Nothing
        f _ (_, []) = Nothing
        f (l1, c1:r1) (l2, c2:r2) = if l1 ++ r1 == l2 ++ r2 
            then Just $ l1 ++ r1
            else f (l1 ++ [c1], r1) (l2 ++ [c2], r2)
        

search :: [String] -> [String] -> String 
search all (x:xs) 
    | Just answer <- f x all = answer
    | otherwise = search all xs
    where        
        f s [] = Nothing
        f s (x:xs) 
            | s == x = Nothing
            | Just answer <- correct s x = Just answer
            | otherwise = f s xs

day02_1 :: IO ()
day02_1 = do
    x <- lines <$> readFile "day02.input"
    print $ search x x