module Day04
    ( day04_0, day04_1
    ) where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe (fromJust)
import Data.HashSet as HS hiding (filter)
import Data.Ord (comparing)
import Data.HashMap as HM hiding (filter)
import Data.List (tails, find, sortBy, maximumBy, elemIndex)
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P
import Data.Dates
import Data.Dates.Internal

data Message = FallsAsleep | WakesUp | OvernightShift Int deriving (Show)

data Record = Record {
    date :: DateTime,
    message :: Message
} deriving (Show)

pRecord :: Parser Record
pRecord = do
    char '['    
    date <- DateTime <$> pYear <* char '-' <*> pMonth  <* char '-' <*> pDay <* char ' ' 
        <*> number 2 99  <* char ':'  <*> number 2 99 <*> return 0
    string "] "
    msg <- P.choice [
        P.string "wakes" >> return WakesUp,
        P.string "falls" >> return FallsAsleep,
        P.string "Guard #" >> P.many1 P.digit >>= return . OvernightShift . read]
    return $ Record date msg    

data Guard = Guard {
    gid :: Int,
    beginsShift :: DateTime,
    events :: [(DateTime, Message)]
} deriving Show

sleepTime :: Guard -> Int 
sleepTime = sleepTime' . events 
    where
        sleepTime' :: [(DateTime, Message)] -> Int 
        sleepTime' ((d0, FallsAsleep):(d1, WakesUp):xs) = minute d1 - minute d0 + sleepTime' xs
        sleepTime' [] = 0
        sleepTime' [(_, FallsAsleep)] = error "Unsupported case"
        sleepTime' unreachable = error $ show unreachable


guards :: [Record] -> Map Int Guard
guards (Record d (OvernightShift n):xs) = guards' xs (Guard n d []) HM.empty
guards _ = error "Events should start with OvernightShift message"

plus (Guard n d e1) (Guard _ _ e2) = Guard n d (e1 ++ e2)

guards' :: [Record] -> Guard -> Map Int Guard -> Map Int Guard
guards' [] g m = (HM.insertWith plus (gid g) g m) 
guards' (Record d (OvernightShift n):xs) g guards = 
    guards' xs (Guard n d []) (HM.insertWith plus (gid g) g guards) 

guards' (Record d msg:xs) g@(Guard _ _ e) guards = 
    guards' xs g{events = e ++ [(d,msg)]} guards

frequent_list :: Guard -> [Int]
frequent_list (Guard _ _ evnts) = top_minute' evnts $ nums 60 0            
        where             
            nums n x = take n $ repeat x
            top_minute' ((d0, FallsAsleep):(d1, WakesUp):xs) lst = 
                let d0' = minute d0
                    d1' = minute d1
                    sleepingHours = nums (d0') 0 ++ nums (d1' - d0') 1 ++ nums (60 - d1') 0
                in 
                    top_minute' xs $ zipWith (+) lst sleepingHours
            top_minute' [] lst = lst            

top_minute :: Guard -> Int
top_minute g = fromJust $ elemIndex max lst
        where             
            lst = frequent_list g
            max = maximum lst
                    
day04_0 :: IO ()
day04_0 = do
    input <- rights <$> liftM (P.parse pRecord "") <$> lines <$> readFile "day04.input"
    let g = guards $ sortBy (comparing date) input    
    let (guard, sleeping) = maximumBy (comparing snd) $ ((,) <$> gid <*> sleepTime) <$> g
    print $ guard * (top_minute (g ! guard))
                    
day04_1 :: IO ()
day04_1 = do
    input <- rights <$> liftM (P.parse pRecord "") <$> lines <$> readFile "day04.input"
    let g = guards $ sortBy (comparing date) input    
    let freq = ((,) <$> gid <*> frequent_list) <$> g
    let freq' = (\(gid, lst) -> let max = maximum lst in (gid, max, fromJust $ elemIndex max lst)) <$> freq
    let (i, f, n) = maximumBy (comparing $ \(_, x, _) -> x) freq'
    print $ i * n
    
    