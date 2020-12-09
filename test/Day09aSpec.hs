module Day09aSpec
    where
import Test.Hspec
import Day09a

spec = do
    describe "possible sums" $ do
        it "tells all the possible sums of 2 the first n numbers of a list" $ do
            possibleSums [1..10] 4 `shouldBe` [3,4,5,6,7]
            possibleSums [42,17,4807,-23,2] 3 `shouldBe` [59,4824,4849]

    describe "firstNotSum" $ do
        it "tells the first number that is not a sum of 2 numbers amongst the n previous numbers" $ do
            firstNotSum [42,17,4807,59,4824,9631,23,17] 3 `shouldBe` 23 
            let ls = [35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576]
            firstNotSum ls 5 `shouldBe` 127

    it "all of this should solve the puzzle" $ do
        ls <- fmap (map read.lines) $ readFile "data/Day09input.txt"
        firstNotSum ls 25 `shouldBe` 29221323
