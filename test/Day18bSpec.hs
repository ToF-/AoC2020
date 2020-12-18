module Day18bSpec
    where
import Test.Hspec
import Day18b
import Data.Maybe

spec = do
    describe "parsing" $ do
        describe "a number" $ do
            it "any single digit will do" $ do
                parse number "4"  `shouldBe` Just 4
                parse number "3"  `shouldBe` Just 3

        describe "parentheses" $ do
            it "enclose an expression" $ do
                parse (parentheses number) "(4)" `shouldBe` Just 4

        describe "a simple expression" $ do
            it "include several operations" $ do 
                parse expression "4+3+2" `shouldBe` Just 9

            it "has reversed precedence of + over *" $ do
                parse expression "4+3*2" `shouldBe` Just 14
                parse expression "3+4+3*2" `shouldBe` Just ((3+4+3)*2)
                parse expression "3*4+3*2" `shouldBe` Just (3*(4+3)*2)

            it "can include subexpressions" $ do 
                parse expression "(7*2)" `shouldBe` Just (7*2)
                parse expression "2+(7*2)" `shouldBe` Just (2+7*2)
                parse expression "(7*2)+3" `shouldBe` Just (7*2+3)
                parse expression "(7*2)+(3*4)" `shouldBe` Just (7*2+3*4)
                parse expression "(7*2)+(3*9)" `shouldBe` Just ((7*2)+(3*9)) 
                parse expression "1+(2*3)+(4*(5+6))" `shouldBe` Just 51

            it "skip spaces" $ do
                parse expression " 4 + 3 * 2 " `shouldBe` Just 14
                parse expression "1 + 3" `shouldBe` Just 4
                parse expression "1 + (2 * 3)" `shouldBe` Just 7
                parse expression "1 + (2 * 3) + (4 * (5 + 6)) " `shouldBe` Just 51
                parse expression "2 * 3 + (4 * 5) " `shouldBe` Just 46
                parse expression "5 + (8 * 3 + 9 + 3 * 4 * 3) " `shouldBe` Just 1445
                parse expression "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) " `shouldBe` Just 669060
                parse expression "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 " `shouldBe` Just 23340

    it "all of this should solve the puzzle" $ do
        input <- fmap lines (readFile "data/Day18input.txt")
        print $ catMaybes $ (map (parse expression) input)
        sum (catMaybes (map (parse expression) input)) `shouldBe` 92173001631556
