module Day19bSpec
    where
import Test.Hspec
import Day19b
import Text.ParserCombinators.ReadP
specb = do
    it "should solve the puzzle part2" $ do
        length solution  `shouldBe` 294
