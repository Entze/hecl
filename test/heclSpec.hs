{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
--import Test.LeanCheck
import Test.Hspec.LeanCheck as LC

import Hecl.Math.NumberTheory

instance Listable (NonNegative Int) where
  list = map NonNegative [0..(maxBound :: Int)]

instance Listable (NonNegative Integer) where
  list = map NonNegative [0..]

instance Listable (Positive Int) where
  list = map Positive [1..(maxBound :: Int)]

instance Listable (Positive Integer) where
  list = map Positive [1..]

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
      context "Given Testcases" $ do
        it "getPath testForest (trajectory 0 0 3 1) ->> \"..#.##.####\"" $
          getPath testForest (trajectory 0 0 3 1) `shouldBe` "..#.##.####"
        it "countTrees . getPath testForest (trajectory 0 0 3 1) ->> 7" $
          (countTrees . getPath testForest) (trajectory 0 0 3 1) `shouldBe` 7
        it "product $ map (countTrees . getPath testForest) (map (uncurry (trajectory 0 0)) trajectorySteps) ->> 336" $
          product (map (countTrees . getPath testForest) (map (uncurry (trajectory 0 0)) trajectorySteps)) `shouldBe` 336


testForest :: Forest
testForest = makeForest ["..##.......","#...#...#..",".#....#..#.","..#.#...#.#",".#...##..#.","..#.##.....",".#.#.#....#",".#........#","#.##...#...","#...##....#",".#..#...#.#"]

trajectorySteps = [(1,1), (3,1), (5,1), (7,1), (1,2)]

propFindPositiveSummandsLengthIsN :: Integral int => NonNegative Int -> Positive int -> [Positive int] -> Bool
propFindPositiveSummandsLengthIsN (NonNegative n) (Positive s) ls' = all ((== n) . length) (findPositiveSummands n s ls)
  where
    ls = map getPositive ls'

propFindPositiveSummandsSumIsS :: Integral int => NonNegative Int -> Positive int -> [Positive int] -> Bool
propFindPositiveSummandsSumIsS (NonNegative n) (Positive s) ls' = all ((== s) . sum) (findPositiveSummands n s ls)
  where
    ls = map getPositive ls'
