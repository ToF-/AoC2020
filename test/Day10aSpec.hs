module Day10aSpec
    where
import Test.Hspec
import Day10a

spec = do
    describe "count diffs" $ do
        it "count  the relative differences" $ do
            let l = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
            countDiffs l `shouldBe`  [(7,1),(5,3)]
            let l = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
            countDiffs l `shouldBe`  [(22,1),(10,3)]
    it "all of this should solve the puzzle" $ do
        l <- fmap (map read.lines) $ readFile "data/Day10input.txt"
        countDiffs l `shouldBe`  [(72,1),(31,3)]
        solve l `shouldBe` 2232
