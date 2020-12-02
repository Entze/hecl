module Hecl.Math.NumberTheory where
{-@ findPositiveSummands :: Integral int => Nat -> Nat -> [Nat] -> [[Nat]] @-}
findPositiveSummands :: Integral int => Int -> int -> [int] -> [[int]]
findPositiveSummands 0 _ _ = []
findPositiveSummands _ _ [] = []
findPositiveSummands n s ls
  | n >= 0 && s > 0 && (length . take (n+1)) ls >= n = findPositiveSummands' n s (filter (<= s) ((filter (> 0)) ls))
  | otherwise = []
  where
    findPositiveSummands' :: Integral int => Int -> int -> [int] -> [[int]]
    findPositiveSummands'  0  _  _ = []
    findPositiveSummands'  _  _ [] = []
    findPositiveSummands'  1  s ls = (map (:[]) . filter (== s)) ls
    findPositiveSummands' n s (l:ls)
      | s < 0             = []
      | null matching     = further
      | otherwise         = (map (l:) matching) ++ further
      where
        further           = findPositiveSummands'     n     s ls
        matching          = findPositiveSummands' (n-1) (s-l) ls
