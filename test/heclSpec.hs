{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
--import Test.LeanCheck
import Test.Hspec.LeanCheck as LC

import Hecl.Math.NumberTheory
import Hecl.Special
import Data.List.Split

instance Listable (NonNegative Int) where
  list = map NonNegative [0..(maxBound :: Int)]

instance Listable (NonNegative Integer) where
  list = map NonNegative [0..]

instance Listable (Positive Int) where
  list = map Positive [1..(maxBound :: Int)]

instance Listable (Positive Integer) where
  list = map Positive [1..]

instance Listable PassportKey where
  list = [(minBound)..]

instance Arbitrary PassportKey where
  arbitrary = elements [(minBound)..]

newtype ValidBirthyear = VByr String deriving (Eq, Ord, Show)

getValidBirthyear (VByr s) = s

instance Listable ValidBirthyear where
  list = map (VByr . show) [1920..2002]

instance Arbitrary ValidBirthyear where
  arbitrary = (elements . map (VByr . show)) [1920..2002]


newtype ValidIssueyear = VIyr String deriving (Eq, Ord, Show)

getValidIssueyear (VIyr s) = s

instance Listable ValidIssueyear where
  list = map (VIyr . show) [1920..2002]

instance Arbitrary ValidIssueyear where
  arbitrary = (elements . map (VIyr . show)) [2010..2020]

newtype ExpirationYear = VEyr String deriving (Eq, Ord, Show)

getValidExpirationYear (VEyr s) = s

instance Listable ValidExpirationyear where
  list = map (VEyr . show) [2020..2030]

instance Arbitrary ValidExpirationyear where
  arbitrary = (elements . map (VEyr . show)) [2020..2030]




main :: IO ()
main = hspec $ do
    describe "Math.NumberTheory.findPositiveSummands" $ do
        context "Given Testcases" $ do
            it "findPositiveSummands 2 2020 [1721,979,366,299,675,1456] ->> [[1721,299]]" $
                findPositiveSummands 2 (2020 :: Int) ([1721,979,366,299,675,1456] :: [Int]) `shouldBe` ([[1721,299]] :: [[Int]])
        context "Derived Testcases" $ do
            it "findPositiveSummands 2 1 [1,0,1] ->> []" $
              findPositiveSummands 2 (1 :: Int) [1,0,1] `shouldBe` []
            it "findPositiveSummands 1 1 [1,1,1] ->> [[1],[1],[1]]" $
              findPositiveSummands 1 (1 :: Int) [1,1,1] `shouldBe` [[1],[1],[1]]
        context "Derived Properties" $ do
            it "all ((== n) . length) findPositiveSummands n _ _ ->> True: LeanCheck" $
              LC.propertyFor 100000 $ (propFindPositiveSummandsLengthIsN :: NonNegative Int -> Positive Integer -> [Positive Integer] -> Bool)
            it "all ((== s) . sum) findPositiveSummands _ s _ ->> True: LeanCheck" $
              LC.propertyFor 100000 $ (propFindPositiveSummandsSumIsS :: NonNegative Int -> Positive Integer -> [Positive Integer] -> Bool)
            modifyMaxSuccess (const 100) $ it "all ((== n) . length) findPositiveSummands n _ _ ->> True: QuickCheck" $
              QC.property $ (propFindPositiveSummandsLengthIsN :: NonNegative Int -> Positive Integer -> [Positive Integer] -> Bool)
            modifyMaxSuccess (const 100) $ it "all ((== s) . sum) findPositiveSummands _ s _ ->> True: QuickCheck" $
              QC.property $ (propFindPositiveSummandsSumIsS :: NonNegative Int -> Positive Integer -> [Positive Integer] -> Bool)
    describe "Special" $ do
      context "Derived Poperties" $ do
        it "not isValidPassport passport || correctNumberOfFields: LeanCheck" $
          LC.propertyFor 100000 $ propValidityImpliesCorrectLength
        modifyMaxSuccess (const 100) $ it "not isValidPassport passport || correctNumberOfFields: QuickCheck" $
          QC.property $ propValidityImpliesCorrectLength

      context "Given Testcases" $ do
        it "getPath testForest (trajectory 0 0 3 1) ->> \"..#.##.####\"" $
          getPath testForest (trajectory 0 0 3 1) `shouldBe` "..#.##.####"
        it "countTrees . getPath testForest (trajectory 0 0 3 1) ->> 7" $
          (countTrees . getPath testForest) (trajectory 0 0 3 1) `shouldBe` 7
        it "product $ map (countTrees . getPath testForest) (map (uncurry (trajectory 0 0)) trajectorySteps) ->> 336" $
          product (map (countTrees . getPath testForest) (map (uncurry (trajectory 0 0)) trajectorySteps)) `shouldBe` 336
        it "correctNumberOfFields firstPassport ->> True" $
          correctNumberOfFields firstPassport `shouldBe` True
        it "correctNumberOfFields secondPassport ->> False" $
          correctNumberOfFields secondPassport `shouldBe` False
        it "correctNumberOfFields thirdPassport ->> True" $
          correctNumberOfFields thirdPassport `shouldBe` True
        it "correctNumberOfFields fourthPassport ->> True" $
          correctNumberOfFields fourthPassport `shouldBe` False
        it "nrOfFieldsWith requiredField firstPassport ->> 7" $
          nrOfFieldsWith requiredField firstPassport `shouldBe` 7
        it "nrOfFieldsWith optionalField firstPassport ->> 1" $
          nrOfFieldsWith optionalField firstPassport `shouldBe` 1
        it "nrOfFieldsWith requiredField secondPassport ->> 6" $
          nrOfFieldsWith requiredField secondPassport `shouldBe` 6
        it "nrOfFieldsWith optionalField secondPassport ->> 1" $
          nrOfFieldsWith optionalField secondPassport `shouldBe` 1
        it "nrOfFieldsWith requiredField thirdPassport ->> 7" $
          nrOfFieldsWith requiredField thirdPassport `shouldBe` 7
        it "nrOfFieldsWith optionalField thirdPassport ->> 0" $
          nrOfFieldsWith optionalField thirdPassport `shouldBe` 0
        it "nrOfFieldsWith requiredField fourthPassport ->> 6" $
          nrOfFieldsWith requiredField fourthPassport `shouldBe` 6
        it "nrOfFieldsWith optionalField fourthPassport ->> 0" $
          nrOfFieldsWith optionalField fourthPassport `shouldBe` 0
        it "makePassport firstRawPassport ->> firstPassport" $
          makePassport firstRawPassport `shouldBe` firstPassport
        it "makePassport secondRawPassport ->> secondPassport" $
          makePassport secondRawPassport `shouldBe` secondPassport
        it "makePassport thirdRawPassport ->> thirdPassport" $
          makePassport thirdRawPassport `shouldBe` thirdPassport
        it "makePassport fourthRawPassport ->> fourthPassport" $
          makePassport fourthRawPassport `shouldBe` fourthPassport
        it "map parseRawPassport (splitOn \"\\n\\n\\n\" rawPassportsData) ->> [firstRawPassport, secondRawPassport, thirdRawPassport, fourthRawPassport]" $
          map parseRawPassport (splitOn "\n\n\n" rawPassportsData) `shouldBe` [firstRawPassport, secondRawPassport, thirdRawPassport, fourthRawPassport]
        it "isValidField (Byr, \"2002\") ->> True" $
          isValidField (Byr, "2002") `shouldBe` True
        it "isValidField (Byr, \"2003\") ->> False" $
          isValidField (Byr, "2003") `shouldBe` False
        it "isValidField (Hgt, \"60in\") ->> True" $
          isValidField (Hgt, "60in") `shouldBe` True
        it "isValidField (Hgt, \"190cm\") ->> True" $
          isValidField (Hgt, "190cm") `shouldBe` True
        it "isValidField (Hgt, \"190in\") ->> False" $
          isValidField (Hgt, "190in") `shouldBe` False
        it "isValidField (Hgt, \"190\") ->> False" $
          isValidField (Hgt, "190") `shouldBe` False
        it "isValidField (Hcl, \"#123abc\") ->> False" $
          isValidField (Hgt, "#123abc") `shouldBe` False
        it "isValidField (Hcl, \"#123abz\") ->> False" $
          isValidField (Hgt, "#123abz") `shouldBe` False
        it "isValidField (Hcl, \"123abc\") ->> False" $
          isValidField (Hgt, "123abc") `shouldBe` False
        it "isValidField (Ecl, \"brn\") ->> True" $
          isValidField (Ecl, "brn") `shouldBe` True
        it "isValidField (Ecl, \"wat\") ->> False" $
          isValidField (Ecl, "wat") `shouldBe` False
        it "isValidField (Pid, \"000000001\") ->> True" $
          isValidField (Pid, "000000001") `shouldBe` True
        it "isValidField (Pid, \"0123456789\") ->> False" $
          isValidField (Pid, "0123456789") `shouldBe` False
        it "isValidPassport validPassport1 ->> True" $
          isValidPassport validPassport1 `shouldBe` True
        it "isValidPassport validPassport2 ->> True" $
          isValidPassport validPassport2 `shouldBe` True
        it "isValidPassport validPassport3 ->> True" $
          isValidPassport validPassport3 `shouldBe` True
        it "isValidPassport validPassport4 ->> True" $
          isValidPassport validPassport4 `shouldBe` True
        it "isValidPassport invalidPassport1 ->> False" $
          isValidPassport invalidPassport1 `shouldBe` False
        it "isValidPassport invalidPassport2 ->> False" $
          isValidPassport invalidPassport2 `shouldBe` False
        it "isValidPassport invalidPassport3 ->> False" $
          isValidPassport invalidPassport3 `shouldBe` False
        it "isValidPassport invalidPassport4 ->> False" $
          isValidPassport invalidPassport4 `shouldBe` False


propFindPositiveSummandsLengthIsN :: Integral int => NonNegative Int -> Positive int -> [Positive int] -> Bool
propFindPositiveSummandsLengthIsN (NonNegative n) (Positive s) ls' = all ((== n) . length) (findPositiveSummands n s ls)
  where
    ls = map getPositive ls'

propFindPositiveSummandsSumIsS :: Integral int => NonNegative Int -> Positive int -> [Positive int] -> Bool
propFindPositiveSummandsSumIsS (NonNegative n) (Positive s) ls' = all ((== s) . sum) (findPositiveSummands n s ls)
  where
    ls = map getPositive ls'

testForest :: Forest
testForest = makeForest ["..##.......","#...#...#..",".#....#..#.","..#.#...#.#",".#...##..#.","..#.##.....",".#.#.#....#",".#........#","#.##...#...","#...##....#",".#..#...#.#"]

trajectorySteps = [(1,1), (3,1), (5,1), (7,1), (1,2)]

rawPassportsData :: String
rawPassportsData = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

firstRawPassport :: [RawPassportEntry]
firstRawPassport = [("ecl","gry"), ("pid","860033327"), ("eyr","2020"), ("hcl","#fffffd"), ("byr","1937"), ("iyr","2017"), ("cid", "147"), ("hgt","183cm")]

secondRawPassport :: [RawPassportEntry]
secondRawPassport = [("iyr","2013"), ("ecl","amb"), ("cid","350"), ("eyr","2023"), ("pid","028048884"), ("hcl","#cfa07d"), ("byr","1929")]

thirdRawPassport :: [RawPassportEntry]
thirdRawPassport = [("hcl","#ae17e1"), ("iyr","2013"), ("eyr","2024"), ("ecl","brn"), ("pid","760753108"), ("byr","1931"), ("hgt","179cm")]

fourthRawPassport :: [RawPassportEntry]
fourthRawPassport = [("hcl","#cfa07d"), ("eyr","2025") ,("pid","166559648"),("iyr","2011"),("ecl","brn"),("hgt","59in")]

firstPassport :: Passport
firstPassport = [(Ecl,"gry"), (Pid,"860033327"), (Eyr,"2020"), (Hcl,"#fffffd"), (Byr,"1937"), (Iyr,"2017"), (Cid, "147"), (Hgt,"183cm")]

secondPassport :: Passport
secondPassport = [(Iyr,"2013"), (Ecl,"amb"), (Cid,"350"), (Eyr,"2023"), (Pid,"028048884"), (Hcl,"#cfa07d"), (Byr,"1929")]

thirdPassport :: Passport
thirdPassport = [(Hcl,"#ae17e1"), (Iyr,"2013"), (Eyr,"2024"), (Ecl,"brn"), (Pid,"760753108"), (Byr,"1931"), (Hgt,"179cm")]

fourthPassport :: Passport
fourthPassport = [(Hcl,"#cfa07d"), (Eyr,"2025") ,(Pid,"166559648"),(Iyr,"2011"),(Ecl,"brn"),(Hgt,"59in")]


invalidPassport1 = [(Eyr,"1972"), (Cid,"100"), (Hcl,"#18171d"), (Ecl,"amb"), (Hgt,"170"), (Pid,"186cm"), (Iyr,"2018"), (Byr,"1926")]
invalidPassport2 = [(Iyr,"2019"), (Hcl,"#602927"), (Eyr,"1967"), (Hgt,"170cm"), (Ecl,"grn"), (Pid,"012533040"), (Byr,"1946")]
invalidPassport3 = [(Hcl,"dab227"), (Iyr,"2012"), (Ecl,"brn"), (Hgt,"182cm"), (Pid,"021572410"), (Eyr,"2020"), (Byr,"1992"), (Cid,"277")]
invalidPassport4 = [(Hgt,"59cm"), (Ecl,"zzz"), (Eyr,"2038"), (Hcl,"74454a"), (Iyr,"2023"), (Pid, "3556412378"), (Byr,"2007")]

validPassport1 = [(Pid, "087499704"), (Hgt,"74in"), (Ecl,"grn"), (Iyr,"2012"), (Eyr,"2030"), (Byr,"1980"), (Hcl,"#623a2f")]

validPassport2 = [(Eyr,"2029"), (Ecl,"blu"), (Cid,"129"), (Byr,"1989"), (Iyr,"2014"), (Pid,"896056539"), (Hcl,"#a97842"), (Hgt,"165cm")]

validPassport3 = [(Hcl,"#888785"), (Hgt,"164cm"), (Byr,"2001"), (Iyr,"2015"), (Cid,"88"), (Pid, "545766238"), (Ecl,"hzl"), (Eyr,"2022")]
validPassport4 = [(Iyr,"2010"), (Hgt,"158cm"), (Hcl,"#b6652a"), (Ecl,"blu"), (Byr,"1944"), (Eyr,"2021"), (Pid,"093154719")]


propValidityImpliesCorrectLength :: Passport -> Bool
propValidityImpliesCorrectLength p = (not . isValidPassport) p || correctNumberOfFields p
