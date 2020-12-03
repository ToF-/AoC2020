module Day03aSpec
    where
import Test.Hspec
import Day03a

spec = describe "trees on slope" $ do
    let p = ["..##......."
            ,"#...#...#.."
            ,".#....#..#."
            ,"..#.#...#.#"
            ,".#...##..#."
            ,"..#.##....."
            ,".#.#.#....#"
            ,".#........#"
            ,"#.##...#..."
            ,"#...##....#"
            ,".#..#...#.#"]
    describe "relative position" $ do
        it "can be within the initial pattern length" $ do
            relPos 3 11 `shouldBe` 3

        it "can be outside the initial pattern length" $ do
            relPos 11 11 `shouldBe` 0
            relPos 17 11 `shouldBe` 6

    describe "is there a tree at this position" $ do
        it "inspecting relative position in pattern" $ do
            isTree 0 0 p `shouldBe` False  
            isTree 0 1 p `shouldBe` True
            isTree 12 4 p `shouldBe` True 
            isTree 13 4 p `shouldBe` False
    describe "count all trees on slope" $ do
        it "counts the trees at relative positions in the pattern" $ do
            countTrees p `shouldBe` 7
    it "shourd solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day03input.txt"
        countTrees  p`shouldBe`  173
