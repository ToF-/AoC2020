module Day11bSpec
    where
import Test.Hspec
import Day11b
import Data.Vector as V

spec = do
    describe "neighbors lines" $ do
        it "tells the coords of neighbors in all directions" $ do
            neighborLines (10,10) (4,7) `shouldBe` 
                [[(5,7),(6,7),(7,7),(8,7),(9,7)]
                ,[(3,7),(2,7),(1,7),(0,7)]
                ,[(4,6),(4,5),(4,4),(4,3),(4,2),(4,1),(4,0)]
                ,[(4,8),(4,9)]
                ,[(3,6),(2,5),(1,4),(0,3)]
                ,[(5,6),(6,5),(7,4),(8,3),(9,2)]
                ,[(3,8),(2,9)]
                ,[(5,8),(6,9)]]

    describe "at" $ do
        it "safely accesses a vector of vectors" $ do
            let v = iterateN 3 (V.map (\x->x*x)) (iterateN 3 (+1) 1)
            toListOfList v `shouldBe` [[1,2,3],[1,4,9],[1,16,81]] 
            v `at` (0,0) `shouldBe` Just 1
            v `at` (2,2) `shouldBe` Just 81
            v `at` (3,-2) `shouldBe` Nothing

    describe "evolve" $ do
        it "tells how an area evolves with regard to occupying seats" $ do
            let input = ["#.##.##.##"
                        ,"#######.##"
                        ,"#.#.#..#.."
                        ,"####.##.##"
                        ,"#.##.##.##"
                        ,"#.#####.##"
                        ,"..#.#....."
                        ,"##########"
                        ,"#.######.#"
                        ,"#.#####.##"]
            let output= ["#.LL.LL.L#"
                        ,"#LLLLLL.LL"
                        ,"L.L.L..L.."
                        ,"LLLL.LL.LL"
                        ,"L.LL.LL.LL"
                        ,"L.LLLLL.LL"
                        ,"..L.L....."
                        ,"LLLLLLLLL#"
                        ,"#.LLLLLL.L"
                        ,"#.LLLLL.L#"]
            evolve input `shouldBe` output 
    describe "stabilize" $ do
        it "iterate on a function until unchanged result" $ do
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
            let output=["#.L#.L#.L#"
                       ,"#LLLLLL.LL"
                       ,"L.L.L..#.."
                       ,"##L#.#L.L#"
                       ,"L.L#.LL.L#"
                       ,"#.LLLL#.LL"
                       ,"..#.L....."
                       ,"LLL###LLL#"
                       ,"#.LLLLL#.L"
                       ,"#.L#LL#.L#"]
            stabilize evolve input `shouldBe` output
            occupied (stabilize evolve input) `shouldBe` 26
    it "all of this should solve the puzzle" $ do
        input <- fmap lines $ readFile "data/Day11input.txt"
        occupied (stabilize evolve input) `shouldBe` 2111
