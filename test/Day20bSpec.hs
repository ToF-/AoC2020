module Day20bSpec
    where

import Test.Hspec
import Day20b
import Data.List
import Data.Maybe


assembly = 
    [[2953,1801,1579,2069,1607,2767,1861,3833,2393,3637,2281,1091]
    ,[1867,3121,3851,3881,3463,3343,3607,1999,3793,2557,3023,3251]
    ,[1783,3701,1657,2383,2129,2039,3001,3709,1433,1559,1321,3067]
    ,[1031,3373,1069,2819,2003,3821,2633,1103,3163,1087,1373,2447]
    ,[2399,2237,3583,1439,2357,2381,3989,3359,3329,2917,3491,3931]
    ,[2341,1367,3191,2591,3181,2693,2297,3137,2153,1201,2027,1093]
    ,[3167,3389,2143,1129,2029,2753,1907,3529,2939,3733,2971,3631]
    ,[1973,1571,3727,3697,3229,2459,1019,2617,1153,1601,3217,1489]
    ,[1951,2551,2539,1723,1747,3187,3581,2857,3307,2707,2389,1699]
    ,[1483,1381,2963,3739,2843,2999,2113,1823,3919,3457,3371,1597]
    ,[3407,3719,3467,1223,3677,3593,1361,1987,3499,3673,3461,1453]
    ,[1049,2677,3623,2251,2521,1789,2011,3257,2089,1997,1913,1709]]
specb = do
    let [t00,t01,t02] = [(1,words "ABC DEF GHI")
                        ,(2,words "CUV FWX IYZ")
                        ,(3,words "V89 XAB ZCD")]
    let [t10,t11,t12] = [(4,words "GHI IJK LTN")
                        ,(5,words "IYZ K01 N23")
                        ,(6,words "ZCD 1EF 3GH")]
    let [t20,t21,t22] = [(7,words "LTN OPQ RST")
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

    describe "originalRow" $ do
       it "tells the exact shapes of a row" $ do
           originalRow [t00,t01,t02] `shouldBe` 
               Just [(1,["ABC","DEF","GHI"]),(2,["CUV","FWX","IYZ"]),(3,["V89","XAB","ZCD"])]

    describe "matchers" $ do
        it "tells the tiles matching a tile" $ do
            map fst (matchers t11 ts) `shouldBe` [2,4,6,8]

    describe "corners" $ do
        it "tells which tiles have a matchcount of 2" $ do
            map fst (corners ts) `shouldBe`[1,3,7,9]
        it "tells the corners of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let result = map fst (corners tiles)
            result `shouldBe` [2953,1091,1049,1709]

    describe "borders" $ do
        it "tells which tiles have a matchcount of 3" $ do
            map fst (borders ts)  `shouldBe` [2,4,6,8]
        it "tells the borders of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let result = map fst (borders tiles)
            length result `shouldBe` 40
            result `shouldBe` [2069,1973,1801,1031,1483,2447,1861,1453,3167,2521,1783,2281,1489,2251,2677,2393,3251,1913,3407,1093,3623,3931,2011,2341,3631,3833,1597,1867,2089,1699,1579,1789,1607,3637,2399,2767,1997,3067,1951,3257]
            

    describe "centers" $ do
        it "tells which tiles have a matchcount of 4" $ do
            map fst (centers ts)  `shouldBe` [5]
        it "tells the centers of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let result = map fst (centers tiles)
            length result `shouldBe` 144 - 40 - 4
            

    describe "borderRows" $ do
        it "tells which tiles are part of a row starting with two given tiles" $ do
            map (map fst) (borderRows t00 t01 ts) `shouldBe` [[1,2,3]]
            map (map fst) (borderRows t00 t10 ts) `shouldBe` [[1,4,7]]
        it "tells the borders of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let corner1 = head $ corners tiles
            let border1 = head $ matchers corner1 tiles
            let result = map (map fst) (borderRows corner1 border1 tiles)
            result `shouldBe` [[2953,1801,1579,2069,1607,2767,1861,3833,2393,3637,2281,1091]]

    describe "nextRows" $ do
        it "tells which tiles are forming the row adjacent to a row" $ do
            let border = head $ borderRows t00 t01 ts
            concatMap (map fst) (nextRows border ts)`shouldBe` [4,5,6]
        it "tells rows of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let corner1 = head $ corners tiles
            let border1 = head $ matchers corner1 tiles
            let row0 = head (borderRows corner1 border1 tiles)
            let result = map (map fst) (nextRows row0 tiles)
            result `shouldBe` [[1867],[3121],[3851],[3881],[3463],[3343],[3607],[1999],[3793],[2557],[3023],[3251]]

    describe "clip" $ do
        it "removes borders from a list of strings" $ do
            let tile = words "ABCDE FGHIJ KLMNO PQRST UVWXY"
            clip tile  `shouldBe` words "GHI LMN QRS"

    describe "assemble" $ do
        it "reassemble pieces from a list of pieces" $ do
            let tts = [[t00,t01,t02],[t10,t11,t12],[t20,t21,t22]]
            let pss = map (map snd) tts
            concatMap assemble pss `shouldBe` 
                ["ABCCUVV89","DEFFWXXAB","GHIIYZZCD","GHIIYZZCD","IJKK011EF","LTNN233GH","LTNN233GH","OPQQ455IJ","RSTT677KL"]

            let tts = [[t00,t01],[t10,t11],[t20,t21]]
            let pss = map (map snd) tts
            concatMap assemble pss `shouldBe` 
                ["ABCCUV","DEFFWX","GHIIYZ","GHIIYZ","IJKK01","LTNN23","LTNN23","OPQQ45","RSTT67"]

    describe "recompose" $ do
        it "creates the original image from original tiles in correct shape" $ do
            recompose (image ts) `shouldBe` words "EWA J0E P4I"
        it "can recompose the image from the puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let picture = recompose (image tiles)
            length (head picture)  `shouldBe` 8 * 12
            length picture `shouldBe` 8 * 12
            putStrLn (unlines picture)

    describe "image" $ do
        it "tells which tiles form the image" $ do
            map (map fst) (image ts) `shouldBe` [[1,2,3],[4,5,6],[7,8,9]]

        it "tells rows of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let result = image tiles

            map (map fst) result `shouldBe` 
                [[2953,1801,1579,2069,1607,2767,1861,3833,2393,3637,2281,1091]
                ,[1867,3121,3851,3881,3463,3343,3607,1999,3793,2557,3023,3251]
                ,[1783,3701,1657,2383,2129,2039,3001,3709,1433,1559,1321,3067]
                ,[1031,3373,1069,2819,2003,3821,2633,1103,3163,1087,1373,2447]
                ,[2399,2237,3583,1439,2357,2381,3989,3359,3329,2917,3491,3931]
                ,[2341,1367,3191,2591,3181,2693,2297,3137,2153,1201,2027,1093]
                ,[3167,3389,2143,1129,2029,2753,1907,3529,2939,3733,2971,3631]
                ,[1973,1571,3727,3697,3229,2459,1019,2617,1153,1601,3217,1489]
                ,[1951,2551,2539,1723,1747,3187,3581,2857,3307,2707,2389,1699]
                ,[1483,1381,2963,3739,2843,2999,2113,1823,3919,3457,3371,1597]
                ,[3407,3719,3467,1223,3677,3593,1361,1987,3499,3673,3461,1453]
                ,[1049,2677,3623,2251,2521,1789,2011,3257,2089,1997,1913,1709]]

        it "tells original rows of the input puzzle" $ do
            input <- fmap lines $ readFile "data/Day20input.txt"
            let tiles =interpret input 
            let result = catMaybes (map originalRow (image tiles))
            sum (map length result) `shouldBe` 144

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

