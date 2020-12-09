module Day09bSpec
    where
import Test.Hspec
import Day09b

spec = do
    describe "possible sums" $ do
        it "tells all the possible sums of 2 the first n numbers of a list" $ do
            possibleSums [1..10] 4 `shouldBe` [3,4,5,6,7]
            possibleSums [42,17,4807,-23,2] 3 `shouldBe` [59,4824,4849]

    describe "firstNotSum" $ do
        it "tells the first number that is not a sum of 2 numbers amongst the n previous numbers" $ do
            firstNotSum [42,17,4807,59,4824,9631,42+17+4807,17] 3 `shouldBe` 4866
            let ls = [35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576]
            firstNotSum ls 5 `shouldBe` 127

    describe "findSubArray" $ do
        it "tells the sub array sum that is equal to n" $ do
            findSubArray [4,3,7] 4 `shouldBe` (0,0)
            findSubArray [4,3,7] 7 `shouldBe` (0,1)
            findSubArray [4,3,7] 14 `shouldBe` (0,2)
            findSubArray [4,3,7] 10 `shouldBe` (1,2)
            findSubArray [42,17,4807,59,23,15,100] (sum [4807,59,23,15]) `shouldBe` (2,5)
            let ls = [35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576]
            findSubArray ls 127 `shouldBe` (2,5)

    describe "solve" $ do
        it "solves the puzzle of subArrays" $ do
            let ls = [35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576]
            solve ls 5 `shouldBe` 62
    it "all of this should solve the puzzle" $ do
        ls <- fmap (map read.lines) $ readFile "data/Day09input.txt"
        solve ls 25 `shouldBe` 4389369


