module Hecl.List.Combinatorics where


sublists :: [a] -> [[a]]
sublists (l:ls) = (l:ls):(sublists ls)
sublists [] = []
