module Day14bSpec
    where
import Test.Hspec
import Day14b
import Data.List

spec = do
    describe "dispatch" $ do
        it "dispatch all bits" $ do
            dispatch "0"  `shouldBe` [0]
            dispatch "1"  `shouldBe` [1]
            dispatch "X" `shouldBe` [0,1]
            dispatch "XX" `shouldBe` [0,1,2,3]
            dispatch "X1"  `shouldBe` [1,3]
            dispatch "X0"  `shouldBe` [0,2]
            dispatch "X10"  `shouldBe` [2,6]
            dispatch "X1101X" `shouldBe` [26,27,58,59]

    describe "showAddress" $ do
        it "shows an address" $ do
            showAddress (32768+7) `shouldBe` "000000000000000000001000000000000111"

    describe "mask" $ do
        it "masks an address" $ do
            mask 42 "000000000000000000000000000000X1001X"
                `shouldBe` "000000000000000000000000000000X1101X"

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
            readAll m `shouldBe` [256,1000,42]

        it "can be written cell-wise" $ do
            let m = memory
            let m'= write m 17 42
            m' `at` 17 `shouldBe` 42
            let m''= write m 17 23
            m'' `at` 17 `shouldBe` 23

    describe "load" $ do
        it "can dispatch a value to addresse" $ do
            let mask = "000000000000000000000000000000X1001X"
            let cells = [(42,100)]
            let m = load memory mask cells
            map (m `at`) [59,58,27,26] `shouldBe` [100,100,100,100]

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
            let input = ["mask = 000000000000000000000000000000X1001X"
                        ,"mem[42] = 100"
                        ,"mask = 00000000000000000000000000000000X0XX"
                        ,"mem[26] = 1"]
            solve input `shouldBe` 208

    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day14input.txt"
        solve input `shouldBe` 5030603328768
