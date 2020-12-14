module Day13bSpec
    where
import Test.Hspec
import Day13b

spec = do
    let table = [(0,Just 7),(1,Just 13),(2,Nothing),(3,Nothing),(4,Just 59),(5,Nothing),(6,Just 31),(7,Just 19)]
    describe "interpret" $ do
        it "ignores the start and reads the bus table " $ do
            let input = ["939"
                        ,"7,13,x,x,59,x,31,19"]
            interpret input `shouldBe` table

    describe "congruences" $ do
        it "tells the congruences from the table" $ do
            congruences table  `shouldBe` [(0,7),(-1,13),(-4,59),(-6,31),(-7,19)]


    describe "euclid" $ do
        it "tells the Bejoux identities u v from a the pgcd of two numbers" $ do
            euclid 1 0  `shouldBe` (1,1,0)
            euclid 2 1  `shouldBe` (1,0,1)
            euclid 16 3 `shouldBe` (1,1,-5)
            euclid 120 23 `shouldBe` (1,-9,47)

    describe "reduce" $ do
        it "reduce a very big congruence number to a smaller one" $ do
            let t = 11676431783
            let k = 3162341
            reduce t k `shouldBe` 1068811

    describe "solveCM" $ do
        it "finds a solution for a congruence system" $ do
            solveCM 0 3 3 4 `shouldBe` -9
            solveCM 0 7 (-1) 13 `shouldBe` -14

    describe "solveCMSystem" $ do
        it "finds a solution to a system of congruence" $ do
            let s = [(0,3),(3,4)] 
            solveCMSystem s `shouldBe` (-9,12)
            (-9) `mod` 3 `shouldBe` 0 `mod` 3
            (-9) `mod` 4 `shouldBe` 3 `mod` 4

            let s = [(14,17),(33,56)] 
            solveCMSystem s `shouldBe` (-7415,952)
            (-7415) `mod` 17 `shouldBe` 14 `mod` 17
            (-7415) `mod` 56 `shouldBe` 33 `mod` 56

            uncurry reduce (solveCMSystem (congruences table))  `shouldBe` 1068781

    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day13input.txt"
        let table = interpret input
        uncurry reduce (solveCMSystem (congruences table))  `shouldBe` 1001569619313439
