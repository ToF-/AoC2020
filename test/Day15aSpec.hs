module Day15aSpec
    where
import Test.Hspec
import Day15a
import Data.Map as M

spec = do
    describe "start" $ do
        it "starts with a set of numbers" $ do
            let (turn,last, memory) = start [0,3,6]
            turn `shouldBe` 4
            last `shouldBe` 0
            M.toList memory `shouldBe` [(0,[1]),(3,[2]),(6,[3])]

    describe "speak" $ do
        it "gives 0 if the number appeared for the first time in the game" $ do
            speak 42 (fromList [(42,[1])]) `shouldBe` 0

        it "gives the difference between last two turns where number was spoken" $ do
            speak 0 (fromList [(0,[4,1]),(3,[2])]) `shouldBe` 3

    describe "play" $ do
        let g = start [0,3,6]
        let (turn,last,memory) = play g

        it "adds a turn in the game" $ do
            turn `shouldBe` 5

        it "speaks the new number" $ do
            last `shouldBe` 3

        it "memorize the last number that was spoken" $ do
            M.toList memory `shouldBe` [(0,[4,1]),(3,[2]),(6,[3])]

    describe "loop" $ do
        it "plays several times" $ do
            let (turn,last,_) = loop 2020 [0,3,6]
            turn `shouldBe` 2020
            last  `shouldBe` 436

    it "all of this should solve the puzzle" $ do
        solve 2020 [0,3,6] `shouldBe` 436
        solve 2020 [1,3,2] `shouldBe` 1
        solve 2020 [2,1,3] `shouldBe` 10
        solve 2020 [1,2,3] `shouldBe` 27
        solve 2020 [2,3,1] `shouldBe` 78
        solve 2020 [3,2,1] `shouldBe` 438
        solve 2020 [3,1,2] `shouldBe` 1836
        solve 2020 [20,9,11,0,1,2] `shouldBe` 1111


