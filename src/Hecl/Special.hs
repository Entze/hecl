module Hecl.Special where

import Data.Maybe
import Data.Char
import Text.Read
import Data.List.Split
import Text.Regex.TDFA
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

type RawPassportData = String
type RawPassportEntry = (String, String)
data PassportKey = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid deriving (Eq, Show, Enum, Read, Bounded)
type PassportEntry = (PassportKey, String)
type Passport = [PassportEntry]

passportKeys :: [PassportKey]
passportKeys = [(minBound)..]
requiredKeys = filter requiredKey passportKeys
optionalKeys = filter optionalKey passportKeys

parseRawPassport :: RawPassportData -> [RawPassportEntry]
parseRawPassport = map (\(h1:h2:_) -> (h1,h2)) . map (splitOn ":") . splitOneOf "\n "

makePassportEntry :: RawPassportEntry -> Maybe PassportEntry
makePassportEntry ((f:k),v) = do
  key <- readMaybe k'
  value <- Just v
  return (key, value)
  where
    k' = (toUpper f):(map toLower k)

makePassport :: [RawPassportEntry] -> Passport
makePassport = map fromJust . filter isJust .  map makePassportEntry

correctNumberOfFields :: Passport -> Bool
correctNumberOfFields = (== (length requiredKeys)) . nrOfFieldsWith requiredField

isValidPassport :: Passport -> Bool
isValidPassport p = isValidPassport' p 0
  where
    isValidPassport' [] n = n == length requiredKeys
    isValidPassport' (e:p) n
      | optionalField e = isValidPassport' p n
      | requiredField e = isValidField e && isValidPassport' p (n+1)
      | otherwise       = False

requiredKey Cid = False
requiredKey _ = True

optionalKey Cid = True
optionalKey _ = False

requiredField = requiredKey . fst
optionalField = optionalKey . fst

isValidField :: PassportEntry -> Bool

isValidField (Byr, y) = y =~ "^19[2-9][0-9]$" || y =~ "^200[0-2]$"
isValidField (Iyr, y) = y =~ "^20[12][0-9]$"
isValidField (Eyr, y) = y =~ "^20[23][0-9]$"
isValidField (Hgt, h) = h =~ "^1[5-8][0-9]cm$" || h =~ "^19[0-3]cm$" || h =~ "^59in$" || h =~ "^6[0-9]in$" || h =~ "^7[0-6]in$"
isValidField (Hcl, c) = c =~ "^#[a-f0-9]{6}$"
isValidField (Ecl, "amb") = True
isValidField (Ecl, "blu") = True
isValidField (Ecl, "brn") = True
isValidField (Ecl, "gry") = True
isValidField (Ecl, "grn") = True
isValidField (Ecl, "hzl") = True
isValidField (Ecl, "oth") = True
isValidField (Ecl, _) = False
isValidField (Pid, n) = n =~ "^[0-9]{9}$"
isValidField (Cid, _) = True

nrOfFieldsWith :: (PassportEntry -> Bool) -> Passport -> Int
nrOfFieldsWith predicate = length . filter predicate
