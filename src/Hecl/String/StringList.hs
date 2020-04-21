module Hecl.String.StringList where

stringList :: (Foldable t, Show a) => t a -> String
stringList = foldr ((++) . show) ""
