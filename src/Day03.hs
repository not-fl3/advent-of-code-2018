module Day03
    ( day03_0, day03_1
    ) where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.HashSet as HS hiding (filter)
import Data.HashMap as HM hiding (filter)
import Data.List (tails, find)
import Text.ParserCombinators.Parsec as P

data Square = Square Int (Int, Int) (Int, Int) 

innerCells :: Square -> [(Int, Int, Int)]
innerCells (Square n (x0, y0) (w, h)) = [(n, x, y) | x <- [x0 .. x0 + w - 1], y <- [y0 .. y0 + h - 1]]

pInt :: Parser Int
pInt = liftM read $ P.many1 P.digit

pSquare :: Parser Square
pSquare = do
    P.char '#'
    n <- pInt
    P.string " @ "
    x <- pInt
    P.char ','
    y <- pInt
    P.string ": "
    w <- pInt
    P.char 'x'
    h <- pInt
    return $ Square n (x, y) (w, h)
    
overlaps:: (Square, Square) -> Set (Int, Int) -> Set (Int, Int) 
overlaps (left, Square _ p1 s1) = 
    let 
        inside (px, py) (x, y) (w, h) = px >= x && px < x + w && py >= y && py < y + h
        cells = [(x, y) | (_, x, y) <- innerCells left, inside (x, y) p1 s1]
    in 
        flip (foldr HS.insert) cells

day03_0 :: IO ()
day03_0 = do
    input <- lines <$> readFile "day03.input"
    let squares = rights $ P.parse pSquare "" <$> input
    let pairs = [(x, y) | (x:ys) <- tails squares, y <- ys]    
    print $ HS.size $ foldr overlaps HS.empty pairs

type Board = Map (Int, Int) (Set Int)

mkBoard :: [(Int, Int, Int)] -> Board
mkBoard lst = foldr f HM.empty lst
    where f (n, x, y) = HM.insertWith HS.union (x, y) (HS.singleton n)

notoverlapped :: Board -> Square -> Bool
notoverlapped board square = f board $ innerCells square
    where
        f _ [] = True
        f b ((n, x, y):xs) = case cell of
                Just(cell) | HS.member n cell && HS.size cell == 1 -> f b xs
                           | otherwise -> False
                Nothing -> f b xs
            where
                cell = HM.lookup (x, y) b

day03_1 :: IO ()
day03_1 = do
    input <- lines <$> readFile "day03.input"
    let squares = rights $ P.parse pSquare "" <$> input
    let board = mkBoard [c | cell <- innerCells <$> squares, c <- cell]    
    let cell = maybe (-1) (\s@(Square c _ _) -> c)  $ find (notoverlapped board) squares
    print cell
