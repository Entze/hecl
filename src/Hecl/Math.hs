module Hecl.Math where

isInRange :: Ord a => a -> a -> a -> Bool
isInRange mi ma e = mi <= e && e <= ma
