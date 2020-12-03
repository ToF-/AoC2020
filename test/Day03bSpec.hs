module Day03bSpec
    where
import Test.Hspec
import Day03b

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
            countTrees (3,1) p `shouldBe` 7
    describe "count trees for slopes" $ do
        it "counts the trees depending on the slope" $ do
            countTrees (3,3) p `shouldBe` 0
            let sls = [(1,1),(3,1),(5,1),(7,1),(1,2)]
            let cts = countTreesForSlopes sls p 
            cts `shouldBe` [2,7,3,4,2]
            product cts  `shouldBe` 336
    it "should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day03input.txt"
        let sls = [(1,1),(3,1),(5,1),(7,1),(1,2)]
        let cts = countTreesForSlopes sls p
        cts `shouldBe` [82,173,84,80,46] 
        product cts `shouldBe` 4385176320 

    
