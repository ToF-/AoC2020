module Day06aSpec
    where
import Test.Hspec
import Day06a

spec = do
    describe "collect" $ do
        it "collects groups of lines separated by an empty line" $ do
            let ls = ["hello"
                     ,"world"
                     ,""
                     ,"foobar"
                     ,"qux"]
            collect ls `shouldBe`
                [["hello","world"]
                ,["foobar","qux"]]

    describe "binary" $ do
        it "converts a string [a-z]* into an Int" $ do
            binary "a" `shouldBe` 1 
            binary "b" `shouldBe` 2
            binary "ad" `shouldBe` 9
    describe "count" $ do
        it "counts the number of distinct letters in a line" $ do
            count ["hello","world"] `shouldBe` 7 
    describe "total" $ do
        it "totalise the number of questions for all groups" $ do
            let p = ["abc"
                    ,""
                    ,"a"
                    ,"b"
                    ,"c"
                    ,""
                    ,"ab"
                    ,"ac"
                    ,""
                    ,"a"
                    ,"a"
                    ,"a"
                    ,"a"
                    ,""
                    ,"b"]
            total p `shouldBe` 11
    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day06input.txt"
        let r = (sum . map count . collect) p
        r `shouldBe` 6878
