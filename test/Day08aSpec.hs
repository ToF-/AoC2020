module Day08aSpec
    where
import Test.Hspec
import Day08a

spec = do
    describe "BootCode" $ do
        it "starts with accumulator = 0" $ do
            accumulator (bootcode []) `shouldBe` 0 


        it "can execute a ACC" $ do
            accumulator (bootcode [ACC 1]) `shouldBe` 1 
            accumulator (bootcode [ACC 42]) `shouldBe` 42

        it "can execute several ACCs" $ do
            accumulator (bootcode [ACC 17
                                  ,ACC 23]) `shouldBe` 40

        it "can execute a NOP" $ do 
            accumulator (bootcode [ACC 17
                                  ,NOP 0
                                  ,ACC 42]) `shouldBe` 59
        it "can execute a JMP" $ do
            accumulator (bootcode [JMP 2
                                  ,ACC 4807
                                  ,ACC 42]) `shouldBe` 42

        it "halts on first loop detection" $ do
            accumulator (bootcode [ACC 42
                                  ,JMP (-1)]) `shouldBe` 42 
            let pgm = [NOP 0
                      ,ACC 1
                      ,JMP 4
                      ,ACC 3
                      ,JMP (-3)
                      ,ACC (-99)
                      ,ACC 1
                      ,JMP (-4)
                      ,ACC 6]
            let bc = bootcode pgm
            reverse (trace bc)  `shouldBe` [0,1,2,6,7,3,4]
            accumulator bc `shouldBe` 5 
    describe "readCode" $ do
        it "interprets code from strings" $ do
            readCode "NOP +8" `shouldBe` (NOP 8)  
            readCode "ACC -8" `shouldBe` (ACC (-8))  
            readCode "JMP +8" `shouldBe` (JMP 8)  

    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day08input.txt"
        let pgm = map readCode p
        let bc = bootcode pgm
        accumulator bc `shouldBe` 1671 



