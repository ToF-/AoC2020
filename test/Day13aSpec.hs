module Day13aSpec
    where
import Test.Hspec
import Day13a

spec = do
    describe "departure" $ do
        it "tells the earliest bus and wait time to depart" $ do
            let start = 939
            let buses =[7,13,59,31,19]
            departure start buses `shouldBe` (59,5)
    describe "interpret" $ do
        it "reads the start and bus table" $ do
            let input = ["939"
                        ,"7,13,x,x,59,x,31,19"]
            interpret input `shouldBe` (939,[7,13,59,31,19])
    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day13input.txt"
        let schedule = interpret input
        let dept = uncurry departure schedule
        dept  `shouldBe` (643,5)
        uncurry (*) dept  `shouldBe` 0
