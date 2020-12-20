module Day19aSpec
    where
import Test.Hspec
import Day19a

speca = do
    it "should solve the puzzle part1" $ do
        length solution  `shouldBe` 142
