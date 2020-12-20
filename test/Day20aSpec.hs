module Day20aSpec
    where

import Test.Hspec
import Day20a
import Data.List

spec = do
    let [t00,t01,t02] = [(1,words "ABC DEF GHI")
                        ,(2,words "CUV FWX IYZ")
                        ,(3,words "V89 XAB ZCD")]
    let [t10,t11,t12] = [(4,words "GHI IJK LtN")
                        ,(5,words "IYZ K01 N23")
                        ,(6,words "ZCD 1EF 3GH")]
    let [t20,t21,t22] = [(7,words "LtN OPQ RST")
                        ,(8,words "N23 Q45 T67")
                        ,(9,words "3GH 5IJ 7KL")]

    let ts = [t00,t01,t02,t10,t11,t12,t20,t21,t22]

    describe "match" $ do
        it "tells if last line of a tile matches first line of a tile" $ do
            t00 `match` t10 `shouldBe` True
            t00 `match` t22 `shouldBe` False


    describe "transformations" $ do
        it "tells all the possible transformations of a tile" $ do
            let tile = (1,words "AB CD")
            transformations tile `shouldBe` 
             [(1,["AB","CD"]),(1,["BD","AC"]),(1,["BA","DC"]),(1,["CD","AB"]),(1,["AC","BD"]),(1,["DB","CA"]),(1,["DC","BA"]),(1,["CA","DB"])]

    describe "possible matches" $ do
        let t1 = (1,words "BA DC")
        let t2 = (2,words "HG DB")
        let t3 = (3,words "XA JV")
        it "tells if a tile matches another tile with transformation" $ do
            t1 `possibleMatches` t2 `shouldBe`
                [((1,["AC","BD"]) ,(2,["BD","GH"]))
                ,((1,["CA","DB"]) ,(2,["DB","HG"]))]
            t1 `possibleMatches` t3 `shouldBe` []
        describe "match count" $ do
            it "tells how mamy possible tiles a tiles matches" $ do
                matchCount t1 [t1,t2,t3] `shouldBe` 1

        describe "corners" $ do
            it "tells which tiles have a matchcount of 2" $ do
                let ts = [(1,reverse $ words "AB CD")
                         ,(2,words "BI DJ")
                         ,(3,reverse $ words "IM JN")
                         ,(4,words "CD EF")
                         ,(5,words "DJ FK")
                         ,(6,words "JW KO")
                         ,(7,words "EF GH")
                         ,(8,words "FK HL")
                         ,(9,map reverse $ words "KO LP")]
                corners ts `shouldBe`  
                    [(1,["CD","AB"]),(6,["JW","KO"]),(7,["EF","GH"]),(9,["OK","PL"])]

    describe "interpret" $ do
        it "interpret input as a list of tiles" $ do
            let t = ["Tile 42:"
                    ,"AB"
                    ,"CD"
                    ,""
                    ,"Tile 2:"
                    ,"EF"
                    ,"GH"
                    ,""]
            interpret t `shouldBe` 
                [(42,["AB","CD"]),(2,["EF","GH"])]

    it "all of this should solve part1" $ do
        input <- fmap lines $ readFile "data/Day20input.txt"
        let tiles =interpret input 
        let result = map fst $ corners tiles
        result `shouldBe` [2953,1091,1049,1709]
        product result  `shouldBe` 5775714912743




