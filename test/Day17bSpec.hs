module Day17bSpec
    where
import Test.Hspec
import Day17b
import Data.List 

spec = do
    let g = grid [(0,-1,0,0),(1,0,0,0),(-1,1,0,0),(0,1,0,0),(1,1,0,0)]
    describe "neighbors" $ do
        it "tells the neighbors of a point in 4 dimensions" $ do
            let ns = sort $ neighbors (0,0,0,0)
            length ns  `shouldBe` 80

    describe "active zone" $ do
        it "tells which zone to consider for evolution" $ do
            let g = grid [(-4,-3,1,-3),(6,2,9,0)]
            activeZone g `shouldBe` (-5,10)

    describe "zwView" $ do
        it "show active cubes for a nxn plane on the z and w axes" $ do
            zwView 0 0 g `shouldBe` [".#."
                                    ,"..#"
                                    ,"###"]
    describe "evolve" $ do
        it "generate active cube according to laws" $ do
            let g' = evolve g 
            zwView (-1) 1 g' `shouldBe` ["#.."
                                        ,"..#"
                                        ,".#."]
    describe "count" $ do
        it "tells how many active element after n cycles" $ do
            count 6 g `shouldBe` 848

    it "all of this should solve the puzzle" $ do

        let p = ["...#..#."
                ,"..##.##."
                ,"..#....."
                ,"....#..."
                ,"#.##...#"
                ,"####..##"
                ,"...##.#."
                ,"#.#.#..."]
        let coords = [(x,y,0,0) | y <- [0..7], x <- [0..7], p!!y!!x == '#']
        count 6 (grid coords) `shouldBe` 0
