module Hecl.Special where

import Data.Maybe

-- Advent of Code 2020

type Tree = Char
type TreeLine = [Tree]
type Forest = [TreeLine]


makeForest :: [[Tree]] -> Forest
makeForest = map cycle


trajectory :: Int -> Int -> Int -> Int -> [(Int, Int)]
trajectory startX startY right down = iterate translate (startX,startY)
  where
    translate (posX, posY) = (posX + right, posY + down)


positionInForest :: Forest -> (Int, Int) -> Maybe Tree
positionInForest f (x, y)
  | length f > y = Just (treeLine !! x)
  | otherwise = Nothing
    where
      treeLine = f !! y

getPath :: Forest -> [(Int, Int)] -> TreeLine
getPath forest = map fromJust . takeWhile (/= Nothing) . map (positionInForest forest)

countTrees :: TreeLine -> Int
countTrees = length . filter (== '#')
