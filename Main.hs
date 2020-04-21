module Main where

import Hecl.Math.NumberTheory.MathSequence
import Hecl.String.StringList

main = do
  let s = blumBlumShubBy 14025256 20300713;
  let w = stringList s;
  putStrLn "Test"
  return 0
