module Day08
    ( day08_0, day08_1
    ) where

import Control.Monad (liftM, replicateM)

import Data.Maybe (fromJust, maybe)
import Data.Either (fromRight)
import Text.Parsec.Combinator as P
import Text.ParserCombinators.Parsec as P

data Node = Node {
    childs :: [Node],
    metadata :: [Int]
}

pInt :: Parser Int
pInt = do 
    x <- liftM read $ P.many1 digit
    P.optionMaybe P.anyChar
    return x

pNode :: Parser Node 
pNode = do
    childsAmount <- pInt    
    metadataAmount <- pInt    
    c <- replicateM childsAmount pNode
    m <- replicateM metadataAmount pInt
    return $ Node c m

countMetadata :: Node -> Int 
countMetadata (Node c m) = sum m + (sum $ countMetadata <$> c)

day08_0 :: IO ()
day08_0 = P.parse pNode "" <$> readFile "day08.input" >>= print . countMetadata . fromRight undefined

nth ::[a] -> Int -> Maybe a
nth lst i | i >= length lst = Nothing
          | otherwise = Just $ lst !! i

countDependantMetadata :: Node -> Int 
countDependantMetadata (Node [] m) = sum m
countDependantMetadata (Node c m) = sum $ maybe 0 countDependantMetadata <$> (nth c) . (+ (-1)) <$> m 

day08_1 :: IO ()
day08_1 =  P.parse pNode "" <$> readFile "day08.input" >>= print . countDependantMetadata . fromRight undefined
