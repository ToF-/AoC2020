module Day08bSpec
    where
import Test.Hspec
import Day08b

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
            status bc  `shouldBe` Halt 5 1
    describe "readCode" $ do
        it "interprets code from strings" $ do
            readCode "NOP +8" `shouldBe` (NOP 8)  
            readCode "ACC -8" `shouldBe` (ACC (-8))  
            readCode "JMP +8" `shouldBe` (JMP 8)  

    describe "loop example" $ do
        it "should have status of halt in 0" $ do
            let p = ["jmp +0" -- changed from "nop +0"
                    ,"acc +1"
                    ,"jmp +4"
                    ,"acc +3"
                    ,"jmp -3"
                    ,"acc -99"
                    ,"acc +1"
                    ,"jmp -4"
                    ,"acc +6"]
            let pgm = map readCode p
            let bc = bootcode pgm
            status bc `shouldBe` Halt 0 0 
    describe "terminates example" $ do
        it "should have status of exit" $ do
            let p = ["nop +0"
                    ,"acc +1"
                    ,"jmp +4"
                    ,"acc +3"
                    ,"jmp -3"
                    ,"acc -99"
                    ,"acc +1"
                    ,"nop -4" -- changed from "jmp -4"
                    ,"acc +6"]
            let pgm = map readCode p
            let bc = bootcode pgm
            status bc `shouldBe` Exit 
            accumulator bc  `shouldBe` 8 
    describe "fixing the code" $ do
        let p = ["nop +0"
                ,"acc +1"
                ,"jmp +4"
                ,"acc +3"
                ,"jmp -3"
                ,"acc -99"
                ,"acc +1"
                ,"jmp -4"
                ,"acc +6"]
        let pgm = map readCode p
        describe "fixPoints" $ do
            it "should find the points in the code where a nop can be switched to a jmp or vice versa" $ do
                fixPoints pgm  `shouldBe`  [0,2,4,7]
        describe "switch a fixPoint" $ do
            it "should switch a nop or a jmp in a given position" $ do
                let pgm' = switchFixPoint pgm 0
                let bc = bootcode pgm'
                let pgm'' = switchFixPoint pgm 7
                let bc' = bootcode pgm''
                status bc `shouldBe` Halt 0 0 
                status bc' `shouldBe` Exit 
                accumulator bc'  `shouldBe` 8 

        describe "fixCode" $ do
            it "should find the fix point which makes the code exit" $ do
                let bc = fixCode pgm
                status bc `shouldBe` Exit
                accumulator bc `shouldBe` 8 

    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day08input.txt"
        let pgm = map readCode p
        let bc = fixCode pgm
        status bc  `shouldBe` Exit 
        accumulator bc `shouldBe` 892



