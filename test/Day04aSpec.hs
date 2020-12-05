module Day04aSpec
    where
import Test.Hspec
import Day04a

spec = do
    describe "collect" $ do
        it "collects groups of lines separated by an empty line" $ do
            let ls = ["hello"
                     ,"world"
                     ,""
                     ,"foo bar"
                     ,"qux"]
            collect ls `shouldBe`
                ["hello world"
                ,"foo bar qux"]

    describe "a field" $ do
        describe "can be a byr" $ do
            it "if it has a byr key" $ do 
                field "byr:1937" `shouldBe` Just (BYR,"1937") 
                field "byr:1965" `shouldBe` Just (BYR,"1965") 
                field "foo:1980" `shouldBe` Nothing 
--            it "if it has 4 digits" $ do
--                field "byr:foo" `shouldBe` Nothing 
--                field "byr:42" `shouldBe` Nothing 


        
