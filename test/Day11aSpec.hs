module Day11aSpec
    where
import Test.Hspec
import Day11a
import Data.Vector as V

spec = do
    describe "neighborsCoords" $ do
        it "tells the coords of neighbors of a position" $ do
            neighbors (4,7) `shouldBe` [(3,6),(3,7),(3,8),(4,6),(4,8),(5,6),(5,7),(5,8)]

    describe "at" $ do
        it "safely accesses a vector of vectors" $ do
            let v = iterateN 3 (V.map (\x->x*x)) (iterateN 3 (+1) 1)
            toListOfList v `shouldBe` [[1,2,3],[1,4,9],[1,16,81]] 
            v `at` (0,0) `shouldBe` Just 1
            v `at` (2,2) `shouldBe` Just 81
            v `at` (3,-2) `shouldBe` Nothing

    describe "count neighbors" $ do
        it "count occupied seats neighboring a position" $ do
            let input = [".L#L"
                        ,"L#.L"
                        ,"L#LL"]
            let v = fromListOfList input
            let a = allNeighbors v
            toListOfList a `shouldBe`
                [[1,2,1,1]
                ,[2,2,3,1]
                ,[2,1,2,0]]

    describe "evolve" $ do
        it "tells how an area evolves with regard to occupying seats" $ do
            let input = ["L.LL.LL.LL"
                        ,"LLLLLLL.LL"
                        ,"L.L.L..L.."
                        ,"LLLL.LL.LL"
                        ,"L.LL.LL.LL"
                        ,"L.LLLLL.LL"
                        ,"..L.L....."
                        ,"LLLLLLLLLL"
                        ,"L.LLLLLL.L"
                        ,"L.LLLLL.LL"]
            let output = ["#.##.##.##"
                         ,"#######.##"
                         ,"#.#.#..#.."
                         ,"####.##.##"
                         ,"#.##.##.##"
                         ,"#.#####.##"
                         ,"..#.#....."
                         ,"##########"
                         ,"#.######.#"
                         ,"#.#####.##"]
            evolve input `shouldBe` output 
            occupied (evolve input) `shouldBe` 71

    describe "stabilize" $ do
        it "iterate on a function until unchanged result" $ do
            stabilize (\n -> if n < 10 then n+1 else n) 10 `shouldBe` 10
            let input = ["L.LL.LL.LL"
                        ,"LLLLLLL.LL"
                        ,"L.L.L..L.."
                        ,"LLLL.LL.LL"
                        ,"L.LL.LL.LL"
                        ,"L.LLLLL.LL"
                        ,"..L.L....."
                        ,"LLLLLLLLLL"
                        ,"L.LLLLLL.L"
                        ,"L.LLLLL.LL"]
            let output=["#.#L.L#.##"
                       ,"#LLL#LL.L#"
                       ,"L.#.L..#.."
                       ,"#L##.##.L#"
                       ,"#.#L.LL.LL"
                       ,"#.#L#L#.##"
                       ,"..L.L....."
                       ,"#L#L##L#L#"
                       ,"#.LLLLLL.L"
                       ,"#.#L#L#.##"]
            stabilize evolve input `shouldBe` output
            occupied (stabilize evolve input) `shouldBe` 37
    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day11input.txt"
        occupied (stabilize evolve input) `shouldBe` 0
