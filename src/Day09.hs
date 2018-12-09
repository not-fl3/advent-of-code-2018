module Day09
    ( day09_0, day09_1
    ) where

import Control.Monad (liftM, replicateM)

import Data.Maybe (fromJust, maybe)
import Data.Either (fromRight)
import Data.HashMap as HM
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P
import Debug.Trace

pInput :: Parser (Int, Int)
pInput = do 
    x <- liftM read $ P.many1 digit
    P.string " players; last marble is worth "    
    y <- liftM read $ P.many1 digit
    return (x, y)

type MarblesStore = [Int]
type Table = [Int]
type MarbleIndex = Int
type ElfIndex = Int 
type ScoreTable = HM.Map Int Int
data GameState = GameState {
    sStore :: MarblesStore,
    sTable :: Table,
    sCurrentMarble :: MarbleIndex,
    sElf :: ElfIndex,
    stable :: ScoreTable
 } deriving (Show)

insertAt list ix n = let (ys, zs) = splitAt (ix + 1) list in  ys ++ [n] ++ zs 

deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

step :: Int -> GameState -> GameState
step elfs (GameState store table ix elf scores) = 
    let 
        lowestMarble = head store
        store' = tail store                    
    in 
        if mod lowestMarble 23 == 0 
            then 
                let 
                    bonusIdx = mod (ix - 6) (length table)
                    bonusMarble = table !! bonusIdx
                    table' = deleteAt bonusIdx table
                    ix'= mod (bonusIdx - 1) (length table')
                    scores' = HM.insertWith (+) elf (lowestMarble + bonusMarble) scores
                in 
                    GameState store' table' ix' (mod (elf + 1) elfs) scores'
            else 
                let ix' = mod (ix + 2) (length table)
                    table' = insertAt table ix' lowestMarble
                in GameState store' table' ix' (mod (elf + 1) elfs) scores

showTable :: Int -> [Int] -> Int -> String
showTable _ [] _ = ""
showTable n (x:xs) ix | ix + 1 == n = "( " ++ show x ++ " ) " ++ showTable (n + 1) xs ix
                      | otherwise = show x ++  " " ++ showTable (n + 1) xs ix

playGame :: (Int, Int) -> Int 
playGame (players, marbles) = 
    let gameState = GameState [1..marbles] [0] 0 (-1) HM.empty
        f' g@(GameState [] _ _ _ _) = g
        f' g = let dbg = show (1 + sElf g) ++ ":\t" ++ showTable 0 (sTable g) (sCurrentMarble g)
                in f' $ step players g
        (GameState _ _ _ _ scores) = f' gameState
    in 
        maximum $ HM.elems scores

day09_0 :: IO ()
day09_0 = P.parse pInput "" <$> readFile "day09.input" >>= print . playGame . fromRight undefined

day09_1 :: IO ()
day09_1 = undefined
