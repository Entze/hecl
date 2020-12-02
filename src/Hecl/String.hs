module Hecl.String where

import Text.Regex.TDFA

extractRegex :: String -> String -> [String]
extractRegex reg str = getAllTextMatches (str =~ reg)
