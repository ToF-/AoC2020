module Day18aSpec
    where
import Test.Hspec
import Day18a

spec = do
    describe "prefixer" $ do
        it "keep digits in place" $ do
            prefixer "1" `shouldBe` "1"
            prefixer "4" `shouldBe` "4"

        it "put + and * ahead of expression" $ do
            prefixer "1+4" `shouldBe` "+14"
            prefixer "3*2" `shouldBe` "*32"

        it "treat ( and ) as sub expression to prefix" $ do
            prefixer "(1)" `shouldBe` "1"
            prefixer "((4))" `shouldBe` "4"
            prefixer "(1+3)*(2+6)" `shouldBe` "*+13+26"
            prefixer "1+(2*3)+(4*(5+6))" `shouldBe` "++1*23*4+56"

        it "treat several operators in the right order" $ do
            prefixer "1*2+3" `shouldBe` "+*123"
            prefixer "1+2*3" `shouldBe` "*+123"
            prefixer "1+2*3+4*5+6" `shouldBe` "+*+*+123456"

        it "eliminate spaces" $ do
            prefixer " 1 + 3 " `shouldBe` "+13"

    describe "evaluate" $ do
        it "evaluate a simple digit" $ do
            evaluate "4" `shouldBe` 4
            evaluate "7" `shouldBe` 7

        it "evaluate an addition" $ do
            evaluate "+12" `shouldBe` 3

        it "evaluate a multiplication" $ do
            evaluate "*32" `shouldBe` 6

        it "evaluate long expressions" $ do
            evaluate "+1*23" `shouldBe` 7
            evaluate "*2+23" `shouldBe` 10
            evaluate "++123" `shouldBe` 6
            evaluate "**234" `shouldBe` 24

    describe "eval" $ do
        it "evaluate an infix expression" $ do
            eval "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` 71
            eval "1 + (2 * 3) + (4 * (5 + 6))"  `shouldBe` 51
            eval "2 * 3 + (4 * 5) " `shouldBe`  26
            eval "5 + (8 * 3 + 9 + 3 * 4 * 3) " `shouldBe`  437
            eval "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) " `shouldBe`  12240
            eval "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 " `shouldBe`  13632

    it "all of this should solve the puzzle" $ do
        input <- fmap lines (readFile "data/Day18input.txt")
        sum (map eval input) `shouldBe` 800602729153

