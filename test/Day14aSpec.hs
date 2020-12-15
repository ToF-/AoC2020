module Day14aSpec
    where
import Test.Hspec
import Day14a
import Data.Bits

spec = do
    describe "andMask" $ do
        it "tells what bits should be set to 0" $ do
            andMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"  `shouldBe` 2 ^ 37 -1
            andMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX0X"  `shouldBe` clearBit (2 ^ 37 -1) 1

    describe "orMask" $ do
        it "tells what bits should be set to 1" $ do
            orMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX1X11" `shouldBe` 11


    describe "applyMask" $ do
        it "apply the and mask and the or mask to a value" $ do
            applyMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11 `shouldBe` 73
            applyMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101 `shouldBe` 101
            applyMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0 `shouldBe` 64


    describe "memory" $ do
        it "can be read cell-wise" $ do
            let m = memory
            m `at` 0 `shouldBe` 0

        it "can be read group-wise" $ do
            let m = (write 
                        (write 
                            (write 
                                (write memory 7 42)
                                3 4807)
                            5 1000)
                        3 256)
            readAll m `shouldBe` [0,0,0,256,0,1000,0,42]

        it "can be written cell-wise" $ do
            let m = memory
            let m'= write m 17 42
            m' `at` 17 `shouldBe` 42
            let m''= write m 17 23
            m'' `at` 17 `shouldBe` 23

    describe "load" $ do
        it "can load a masked value in memory" $ do
            let mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            let cells = [(8,11)
                        ,(7,101)
                        ,(8,0)]
            let m = load memory mask cells
            readAll m `shouldBe` [0,0,0,0,0,0,0,101,64]

    describe "interpret" $ do
        it "interpret text as load commands" $ do
            let input = 
                    ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                    ,"mem[8] = 11"
                    ,"mem[7] = 101"
                    ,"mem[8] = 0"]
            interpret input  `shouldBe` ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                                               ,[(8,11),(7,101),(8,0)])

    describe "groupLoad" $ do
        it "regroups input into load commands" $ do
            let input = ["mask = 00000X110010111111X000100XX01010000X"
                        ,"mem[20690] = 435"
                        ,"mem[54036] = 231"
                        ,"mem[27099] = 118644255"
                        ,"mem[55683] = 22299263"
                        ,"mem[26119] = 2279399"
                        ,"mask = 00X000X0001X111111101X1111XX11X001XX"
                        ,"mem[42072] = 1658073"
                        ,"mem[63234] = 2277"
                        ,"mask = 1001X010011011111110101101X0XX11X010"
                        ,"mem[31090] = 52291"
                        ,"mem[31244] = 377352406"
                        ,"mem[10621] = 18801757"
                        ,"mem[31666] = 5100853"]
            let gs = groupLoad input
            gs `shouldBe` [["mask = 00000X110010111111X000100XX01010000X"
                           ,"mem[20690] = 435"
                           ,"mem[54036] = 231"
                           ,"mem[27099] = 118644255"
                           ,"mem[55683] = 22299263"
                           ,"mem[26119] = 2279399"]
                          ,["mask = 00X000X0001X111111101X1111XX11X001XX"
                           ,"mem[42072] = 1658073"
                           ,"mem[63234] = 2277"]
                          ,["mask = 1001X010011011111110101101X0XX11X010"
                           ,"mem[31090] = 52291"
                           ,"mem[31244] = 377352406"
                           ,"mem[10621] = 18801757"
                           ,"mem[31666] = 5100853"]]
    describe "solve" $ do
        it "interprets all load commands" $ do
            let input = ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                         ,"mem[14] = 11"
                         ,"mem[13] = 101"
                         ,"mem[14] = 0"
                        ,"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                         ,"mem[8] = 11"
                         ,"mem[7] = 101"
                         ,"mem[8] = 0"]
            solve input `shouldBe` 330
    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day14input.txt"
        solve input `shouldBe` 4297467072083
