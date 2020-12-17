module Day17aSpec
    where
import Test.Hspec
import Day17a

spec = do
    describe "neighbors" $ do
        it "tells the neighbors of a point in 3 dimensions" $ do
            neighbors (1,2,3) `shouldBe` 
                    [(0,1,2),(0,1,3),(0,1,4),(0,2,2),(0,2,3),(0,2,4),(0,3,2),(0,3,3),(0,3,4),(1,1,2),(1,1,3),(1,1,4),(1,2,2),(1,2,4),(1,3,2),(1,3,3),(1,3,4),(2,1,2),(2,1,3),(2,1,4),(2,2,2),(2,2,3),(2,2,4),(2,3,2),(2,3,3),(2,3,4)]

    describe "zView" $ do
        it "show active cubes for a nxn plane on the z axis" $ do
            let g = set (-2,2,0) $ set (-1,-1,0) initial
            zView (0,0,0) 5 g `shouldBe` ["....."
                                         ,".#..."
                                         ,"....."
                                         ,"....."
                                         ,"#...."]

