module Day02aSpec
    where

import Test.Hspec
import Day02a

spec = describe "valid password" $ do
    it "should read the parameters" $ do
        readParameters "1-3 a: abcde" `shouldBe` 
            (1,3,'a',"abcde")

    it "should validate the password" $ do
        validate (1,3,'a',"abcde") `shouldBe` True 
        validate (1,3,'b',"cdefg") `shouldBe` False 
        validate (2,9,'c',"ccccccccc") `shouldBe` True


    it "should process an entry" $ do
        validPassword "1-3 a: abcde"  `shouldBe` True 


    it "shourd solve the puzzle" $ do
        pwds <- fmap lines $ readFile "./data/Day02input.txt"
        length pwds  `shouldBe`  1000
        howManyValid pwds `shouldBe`  625
