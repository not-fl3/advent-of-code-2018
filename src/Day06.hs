module Day06
    ( day06_0, day06_1
    ) where

import Control.Monad (liftM)

import Data.Ord (comparing)
import Data.Either (rights)
import Data.List (maximumBy, minimumBy)
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P
import Data.Array (Array, assocs, array, listArray, bounds, elems)

pCoord :: Parser (Int, Int)
pCoord = do 
    x <- liftM read $ P.many1 digit
    P.string ", "
    y <- liftM read $ P.many1 digit
    return (x, y)

manhattan :: (Int, Int) -> (Int, Int) -> Int 
manhattan (x0, y0) (x1, y1) = (abs $ x1 - x0) + (abs $ y1 - y0)

solve :: Array (Int, Int) Int -> (Int, (Int, Int)) -> Maybe Int 
solve points (n, p) = 
    let 
        ((lx, ly), (rx, ry)) = Data.Array.bounds points
        border = length . filter (\((px, py), n') -> n' == n && (px == lx || px == rx || py == ly || py == ry)) $ assocs points
        infinite = border /= 0
    in
        if infinite then Nothing else Just $ length . filter (==n) $ elems points

bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds points = ((ltx, lty), (rbx, rby))
    where 
        ltx = minimum $ fst <$> points
        lty = minimum $ snd <$> points
        rbx = maximum $ fst <$> points
        rby = maximum $ snd <$> points

closest :: [(Int, (Int, Int))] -> (Int, Int) -> [Int]
closest points p = 
    let dist = manhattan p $ snd $ minimumBy (comparing $ manhattan p . snd) points
    in fst <$> filter (\(_, x) -> manhattan x p == dist) points
    
day06_0 :: IO ()
day06_0 = do
    points <- zip [1..] <$> rights <$> liftM (P.parse pCoord "") <$> lines <$> readFile "day06.input"    
    let b@(l, r) = Day06.bounds $ snd <$> points    
    let f [x] = x
        f _ = 0
    let arr = array b [((x, y), f $ closest points (x, y)) | x <- [fst l .. fst r], y <- [snd l .. snd r]]
    print $ maximum [x | Just x <- solve arr <$> points]

day06_1 :: IO ()
day06_1 = do
    points <- rights <$> liftM (P.parse pCoord "") <$> lines <$> readFile "day06.input"    
    let distances = (\x -> sum (manhattan x <$> points) < 10000) <$> [(x, y) | x <- [-100 .. 500], y <- [-100 .. 500]]
    print $ length $ filter id distances
    

    
