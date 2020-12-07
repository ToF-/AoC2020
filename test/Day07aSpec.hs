module Day07aSpec
    where
import Test.Hspec
import Day07a

spec = do
    let r = "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    let n = "faded blue bags contain no other bags."
    describe "simplify rule" $ do
        it "remove syntactic elements" $ do
            simplify r `shouldBe` words "dark orange 3 bright white 4 muted yellow" 
            simplify n `shouldBe` words "faded blue no other" 
    
    describe "extract rule" $ do
        it "convert phrase into a tree" $ do
            extract (simplify r) `shouldBe` ("dark orange",[("bright white",3),("muted yellow",4)]) 
            extract (simplify n) `shouldBe` ("faded blue",[]) 

    describe "containers" $ do
        it "tells the bags that can be a container of a given bag" $ do
            let ss =["light red bags contain 1 bright white bag, 2 muted yellow bags."
                    ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                    ,"bright white bags contain 1 shiny gold bag."
                    ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                    ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                    ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                    ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                    ,"faded blue bags contain no other bags."
                    ,"dotted black bags contain no other bags."]
            let rs =  map (extract . simplify) ss

            containers "shiny gold" rs `shouldBe` 
                ["bright white","dark orange","light red","muted yellow","shiny gold"]
            countContainers "shiny gold" ss `shouldBe` 4 
    
    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day07input.txt"
        countContainers "shiny gold" p `shouldBe` 254 
