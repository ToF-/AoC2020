module Day05aSpec
    where
import Test.Hspec
import Day05a

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
        let r = maximum $ map seatId p
        r `shouldBe` 864
