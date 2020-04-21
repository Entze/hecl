module Hecl.Math.NumberTheory.MathSequence where

blumBlumShubBy :: Integral int => int -> int -> [int]
blumBlumShubBy seed modulo = iterate (\n -> (n^2) `rem` modulo) seed
