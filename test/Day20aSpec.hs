module Day20aSpec
    where

import Test.Hspec
import Day20a
import Data.List

spec = do
    let [m00,m01,m02] = [words "ABC DEF GHI"
                        ,words "CUV FWX IYZ"
                        ,words "V89 XAB ZCD"]
    let [m10,m11,m12] = [words "GHI IJK LMN"
                        ,words "IYZ K01 N23"
                        ,words "ZCD 1EF 3GH"]
    let [m20,m21,m22] = [words "LMN OPQ RST"
                        ,words "N23 Q45 T67"
                        ,words "3GH 5IJ 7KL"]

    let m = [[m00,m01,m02],[m10,m11,m12],[m20,m21,m22]]
    let [w00,w01,w02] = [words "ABC DEF GHI"
                        ,words "CUV FWY IYZ"
                        ,words "V89 XAB ZCD"]
    let [w10,w11,w12] = [words "GHI IJK LwN"
                        ,words "IYZ K00 N23"
                        ,words "ZCD 1EF 3GH"]
    let [w20,w21,w22] = [words "LwN OPQ RST"
                        ,words "N23 Q45 T67"
                        ,words "3GH 5IJ 7KL"]

    let w = [[w00,w01,w02],[w10,w11,w12],[w20,w21,w22]]

    describe "match" $ do
        it "tells if last line of a tile matches first line of a tile" $ do
            m00 `match` m10 `shouldBe` True
            m00 `match` m22 `shouldBe` False

    describe "allMatch" $ do
        it "tells if a list of tiles match together" $ do
            allMatch [m00,m10,m20] `shouldBe` True
            allMatch [m10,m20,m11] `shouldBe` False

    describe "valid" $ do
        it "tells if a mosaic has rows and columns matching" $ do
            valid m `shouldBe` True
            valid (reverse m) `shouldBe` False

            valid w `shouldBe` False

    describe "transformations" $ do
        it "tells all the possible transformations of a tile" $ do
            let tile = words "AB CD"
            transformations tile `shouldBe` 
             [["AB","CD"],["BD","AC"],["BA","DC"],["CD","AB"],["AC","BD"],["DB","CA"],["DC","BA"],["CA","DB"]]

    describe "possible matches" $ do
        let t1 = words "BA DC"
        let t2 = words "HG DB"
        let t3 = words "XA JV"
        it "tells if a tile matches another tile with transformation" $ do
            let t1 = words "BA DC"
            let t2 = words "HG DB"
            let t3 = words "XA JV"
            t1 `possibleMatches` t2 `shouldBe`
                [(["AC","BD"] ,["BD","GH"])
                ,(["CA","DB"] ,["DB","HG"])]
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
                    []

