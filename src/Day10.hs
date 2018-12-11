module Day10
    ( day10_0, day10_1
    ) where

import Control.Monad (liftM)

import Data.List (any)
import Data.Either (fromRight, rights)
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P

type Position = (Int, Int)
type Velocity = (Int, Int)
type Point = (Position, Velocity)

pInt :: Parser Int 
pInt = do
    P.skipMany $ P.char ' '
    sign <- P.option (-1) (P.char '-' >> return 1) 
    x <- liftM read $ P.many1 P.digit
    return $ x * sign

pInput :: Parser Point
pInput = do 
    P.string "position=<"    
    x <- pInt
    P.string ","
    y <- pInt
    P.string "> velocity=<"
    vx <- pInt
    P.string ","
    vy <- pInt
    return ((x, y), (vx, vy))

bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds points = ((ltx, lty), (rbx, rby))
    where 
        ltx = minimum $ fst <$> points
        lty = minimum $ snd <$> points
        rbx = maximum $ fst <$> points
        rby = maximum $ snd <$> points

step :: [Point] -> [Point]
step [] = []
step (((x, y), (dx, dy)):xs) = (((x + dx, y + dy), (dx, dy)):step xs)

emptyRow :: Int -> [Point] -> Bool
emptyRow _ [] = True
emptyRow row (((x, y), _):xs) = x /= row && emptyRow row xs

check :: [Point] -> Bool
check points =
    let ((x0, y0), (x1, y1)) = bounds $ fst <$> points
        empty = flip emptyRow points
        space = \row -> empty row && empty (row + 1)
        lettersCheck = length (filter space [x0 .. x1]) >= 1
        spacesCheck = not $ any (\row -> empty row && empty (row + 1) && empty (row + 2)) [x0 .. x1]
    in 
        y1 - y0 < 30 && lettersCheck && spacesCheck

solve :: Int ->[Point] -> (Int, [Point])
solve 100000 _ = error "Failed"
solve n points | check points = (n, points)
               | otherwise = solve (n + 1) $ step points

showMessage :: [Point] -> [String]
showMessage points = 
    let 
        points' = fst <$> points
        ((x0, y0), (x1, y1)) = bounds $ points'
        line y = [if any (\(x', y') -> y' == y && x' == x) points' then '#' else '.' | x <- reverse [x0 .. x1]]
    in [line y |y <- reverse [y0 .. y1]]

day10_0 :: IO ()
day10_0 = rights <$> liftM (P.parse pInput "") <$> lines <$> readFile "day10.input" 
    >>= sequence_ . liftM putStrLn . showMessage . snd . solve 0 
        
day10_1 :: IO ()
day10_1 = rights <$> liftM (P.parse pInput "") <$> lines <$> readFile "day10.input" 
    >>= print . fst . solve 0 
    