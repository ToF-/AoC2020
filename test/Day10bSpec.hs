module Day10bSpec
    where
import Test.Hspec
import Day10b

spec = do
    let l = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    describe "paths" $ do
        it "find the paths between numbers" $ do
            paths l `shouldBe` [(0,[1]),(1,[4]),(4,[5,6,7]),(5,[6,7]),(6,[7]),(7,[10]),(10,[11,12]),(11,[12]),(12,[15]),(15,[16]),(16,[19]),(19,[22]),(22,[])]

    describe "total paths" $ do
        it "compute the total possible paths" $ do
            totalPaths l `shouldBe`
                [(0,8),(1,8),(4,8),(5,4),(6,2),(7,2),(10,2),(11,1),(12,1),(15,1),(16,1),(19,1),(22,1)]
            let l = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
            totalPaths l `shouldBe`
                [(0,19208),(1,10976),(2,5488),(3,2744),(4,2744),(7,2744),(8,1568),(9,784),(10,392),(11,392),(14,392),(17,392),(18,196),(19,98),(20,98),(23,98),(24,49),(25,49),(28,49),(31,49),(32,28),(33,14),(34,7),(35,7),(38,7),(39,7),(42,7),(45,7),(46,4),(47,2),(48,1),(49,1),(52,1)]
    it "all of this should solve the puzzle" $ do
        l <- fmap (map read.lines) $ readFile "data/Day10input.txt"
        solve l `shouldBe` Just 173625106649344
