module Day07
    ( day07_0, day07_1
    ) where

import Control.Monad (liftM)

import Data.Char (ord)
import Data.List (find, delete, sort, nub)
import Data.Maybe (fromJust)
import Data.HashSet as HS hiding (delete, filter)
import Data.HashMap as HM hiding (delete, filter)
import Data.Either (rights)
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P

pStep :: Parser (Char, Char)
pStep = do 
    string "Step "
    x <- P.anyChar
    string " must be finished before step "
    y <- P.anyChar 
    return (x, y)

type Graph = HM.Map Char [Char]
type CompletedTasks = HS.Set Char
type Worker = (Char, Int)

mkGraph :: [(Char, Char)] -> Graph
mkGraph lst = foldr (\(from, to) -> HM.insertWith (++) to [from]) HM.empty lst

ready :: Graph -> CompletedTasks -> [Char] -> [Char] 
ready graph done chars =
    let isReady c =  HM.member c graph == False || (length $ filter (not . flip HS.member done) (graph ! c)) == 0
    in sort $ filter isReady chars

plan :: Graph -> CompletedTasks -> [Char] -> [Char] 
plan _ _ [] = []
plan graph done chars = let r = (ready graph done chars) !! 0
                        in r:plan graph (HS.insert r done) (delete r chars)

day07_0 :: IO ()
day07_0 = do
    steps <- rights <$> liftM (P.parse pStep "") <$> lines <$> readFile "day07.input"
    let chars = nub $ concatMap (\(a, b) -> [a, b]) steps
    print $ plan (mkGraph steps) HS.empty chars


maxWorkers = 5

jobTime :: Char -> Int 
jobTime c = ord c - ord 'A' +  1 + 60

tickWorkers :: [Worker] -> ([Char], [Worker])
tickWorkers workers = 
    let
        workers' = (\(c, t) -> (c, t - 1)) <$> workers
        readyJobs = fst <$> filter ((==0) . snd) workers'
    in 
        (readyJobs, filter ((/=0) . snd) workers')

tick :: [Worker] -> Graph -> CompletedTasks -> [Char] -> Int
tick [] _ _ [] = 0
tick workers graph done chars = 
    let
        (readyJobs, workers') = tickWorkers workers
        chars' = foldr delete chars readyJobs
        done' = done `HS.union` HS.fromList readyJobs
        availableTasks = ready graph done' chars'
        newTasksCount = (max maxWorkers $ length availableTasks) - length workers'
        newTasks = take newTasksCount availableTasks        
        workers'' = workers' ++ ((\c -> (c, jobTime c)) <$> newTasks)
        chars'' = foldr delete chars' newTasks        
    in 
        1 + tick workers'' graph done' chars''

day07_1 :: IO ()
day07_1 = do
    steps <- rights <$> liftM (P.parse pStep "") <$> lines <$> readFile "day07.input"
    let chars = nub $ concatMap (\(a, b) -> [a, b]) steps
    print $ (tick [] (mkGraph steps) HS.empty chars) - 1
