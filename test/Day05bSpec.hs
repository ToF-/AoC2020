module Day05bSpec
    where
import Test.Hspec
import Day05b
import Data.List

spec = do
    describe "seatID" $ do
        it "tells the seatID" $ do
            seatId "FBFBBFFRLR" `shouldBe` 357 
            seatId "FBFBBFFRRR" `shouldBe` 359 
            seatId "BFFFBBFRRR" `shouldBe` 567
            seatId "FFFBBBFRRR" `shouldBe` 119
            seatId "BBFFBBFRLL" `shouldBe` 820
    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day05input.txt"
        let r = findMissingId $ map seatId p 
        r `shouldBe` 739

        
