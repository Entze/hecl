

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
--import Test.LeanCheck
import Test.Hspec.LeanCheck as LC

import Hecl.Math.NumberTheory



main :: IO ()
main = hspec $ do
    describe "Math.NumberTheory.findSummands" $ do
        context "Given Testcases" $ do
            it "findSummands 2 2020 [1721,979,366,299,675,1456] ->> [[1721,299]]" $
                findSummands 2 (2020 :: Int) ([1721,979,366,299,675,1456] :: [Int]) `shouldBe` ([[1721,299]] :: [[Int]])
        context "Derived Testcases" $ do
            it "findSummands 2 1 [1,0,1] ->> [[1,0],[0,1]]" $
              findSummands 2 (1 :: Int) [1,0,1] `shouldBe` [[1,0],[0,1]]
            it "findSummands 1 1 [1,1,1] ->> [[1],[1],[1]]" $
              findSummands 1 (1 :: Int) [1,1,1] `shouldBe` [[1],[1],[1]]
            it "findSummands 1 2 [1,1,1,1] ->> [[1,1],[1,1],[1,1],[1,1],[1,1],[1,1]]" $
              findSummands 1 (2 :: Int) [1,1,1,1] `shouldBe` [[1,1],[1,1],[1,1],[1,1],[1,1],[1,1]]
        context "Derived Properties" $ do
            it "all ((== n) . length) findSummands n _ _ ->> True: LeanCheck" $
              LC.propertyFor 10000 $ (propFindSummandsOnlyProducesCorrectLengthAnswers' :: Int -> Integer -> [Integer] -> Bool)
            modifyMaxSuccess (const 20) $ it "all ((== n) . length) findSummands n _ _ ->> True: QuickCheck" $
              QC.property $ (propFindSummandsOnlyProducesCorrectLengthAnswers :: NonNegative Int -> Integer -> [Integer] -> Bool)
            it "all ((== s) . sum) findSummands _ s _ ->> True: LeanCheck" $
              LC.propertyFor 10000 $ (propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer' :: Int -> Integer -> [Integer] -> Bool)
            modifyMaxSuccess (const 20) $ it "all ((== s) . sum) findSummands _ s _ ->> True: QuickCheck" $
              QC.property $ (propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer :: NonNegative Int -> Integer -> [Integer] -> Bool)

propFindSummandsOnlyProducesCorrectLengthAnswers :: Integral int => NonNegative Int -> int -> [int] -> Bool
propFindSummandsOnlyProducesCorrectLengthAnswers (NonNegative n) s ls = all ((== n) . length) (findSummands n s ls)

propFindSummandsOnlyProducesCorrectLengthAnswers' :: Integral int => Int -> int -> [int] -> Bool
propFindSummandsOnlyProducesCorrectLengthAnswers' n s ls = (n >= 0) LC.==> (all ((== n) . length) (findSummands n s ls))

propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer :: Integral int => NonNegative Int -> int -> [int] -> Bool
propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer (NonNegative n) s ls = all ((== s) . sum) (findSummands n s ls)

propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer' :: Integral int => Int -> int -> [int] -> Bool
propFindSummandsOnlyProducesAnswersWhichSumIsTheSearchedAnswer' n s ls = (n >= 0) LC.==> (all ((== s) . sum) (findSummands n s ls))
