module Day17bSpec
    where
import Test.Hspec
import Day17b

spec = do
    describe "neighbors" $ do
        it "tells the neighbors of a point in 3 dimensions" $ do
            neighbors (1,2,3) `shouldBe` 
                    [(0,1,2),(0,1,3),(0,1,4),(0,2,2),(0,2,3),(0,2,4),(0,3,2),(0,3,3),(0,3,4),(1,1,2),(1,1,3),(1,1,4),(1,2,2),(1,2,4),(1,3,2),(1,3,3),(1,3,4),(2,1,2),(2,1,3),(2,1,4),(2,2,2),(2,2,3),(2,2,4),(2,3,2),(2,3,3),(2,3,4)]

    describe "zView" $ do
        it "show active cubes for a nxn plane on the z axis" $ do
            let g = set (-2,-2,0) $ set (-1,1,0) initial
            zView 0 g `shouldBe` ["#..."
                                 ,"...."
                                 ,"...."
                                 ,".#.."]

    describe "evolve" $ do
        it "generate active cube according to laws" $ do
            let coords = [(0,-1,0),(1,0,0),(-1,1,0),(0,1,0),(1,1,0)]
            let g = grid coords
            zView 0 g `shouldBe` [".#."
                                 ,"..#"
                                 ,"###"]
            let g' = evolve g
            zView (-1) g' `shouldBe` 
                ["#.."
                ,"..#"
                ,".#."]

            zView 0 g' `shouldBe` 
                ["#.#"
                ,".##"
                ,".#."]

            zView 1 g' `shouldBe` 
                ["#.."
                ,"..#"
                ,".#."]
            let g'' = evolve $ evolve $ evolve g
            zView 0 g''  `shouldBe` 
                ["...#..."
                ,"......."
                ,"#......"
                ,"......."
                ,".....##"
                ,".##.#.."
                ,"...#..."]
            zView 1 g''  `shouldBe` 
                ["..#...."
                ,"...#..."
                ,"#......"
                ,".....##"
                ,".#...#."
                ,"..#.#.."
                ,"...#..."]
            zView 2 g''  `shouldBe` 
                ["##."
                ,"###"
                ,"..."]

    describe "count" $ do
        it "tells how many active element after n cycles" $ do
            let coords = [(0,-1,0),(1,0,0),(-1,1,0),(0,1,0),(1,1,0)]
            count 3 (grid coords) `shouldBe` 38
            count 6 (grid coords) `shouldBe` 112

    it "all of this should solve the puzzle" $ do

        let p = ["...#..#."
                ,"..##.##."
                ,"..#....."
                ,"....#..."
                ,"#.##...#"
                ,"####..##"
                ,"...##.#."
                ,"#.#.#..."]
        let coords = [(x,y,0) | y <- [0..7], x <- [0..7], p!!y!!x == '#']
        count 6 (grid coords) `shouldBe` 2

