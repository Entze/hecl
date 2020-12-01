module Hecl.Math.NumberTheory where

findSummands :: Integral int => Int -> int -> [int] -> [[int]]
findSummands 0 _ _ = []
findSummands _ _ [] = []
findSummands _ 0 _ = []
findSummands 1 s ls = (map (:[]) . filter (== s)) ls
findSummands n s (l:ls)
  | null matching = further
  | otherwise = (map (l:) matching) ++ further
  where
    further = findSummands n s ls
    matching = findSummands (n-1) (s-l) ls
