module Day07bSpec
    where
import Test.Hspec
import Day07b

spec = do
    let ss =["light red bags contain 1 bright white bag, 2 muted yellow bags."
            ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
            ,"bright white bags contain 1 shiny gold bag."
            ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
            ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
            ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
            ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
            ,"faded blue bags contain no other bags."
            ,"dotted black bags contain no other bags."]
    let r = "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    let n = "faded blue bags contain no other bags."
    describe "simplify rule" $ do
        it "remove syntactic elements" $ do
            simplify r `shouldBe` words "dark orange 3 bright white 4 muted yellow" 
            simplify n `shouldBe` words "faded blue no other" 
    
    describe "rules" $  do
        it "convert phrases into a list of rules" $ do
            rules ss  `shouldBe`
                [("light red",[("bright white",1),("muted yellow",2)])
                ,("dark orange",[("bright white",3),("muted yellow",4)])
                ,("bright white",[("shiny gold",1)])
                ,("muted yellow",[("shiny gold",2),("faded blue",9)])
                ,("shiny gold",[("dark olive",1),("vibrant plum",2)])
                ,("dark olive",[("faded blue",3),("dotted black",4)])
                ,("vibrant plum",[("faded blue",5),("dotted black",6)])
                ,("faded blue",[]),("dotted black",[])]

    describe "contained bags" $ do
        it "tells how many bags a bag must contain" $ do
            let ss1 = ["shiny gold bags contain 2 dark red bags."
                      ,"dark red bags contain 2 dark orange bags."
                      ,"dark orange bags contain 2 dark yellow bags."
                      ,"dark yellow bags contain 2 dark green bags."
                      ,"dark green bags contain 2 dark blue bags."
                      ,"dark blue bags contain 2 dark violet bags."
                      ,"dark violet bags contain no other bags."]
            containedBags "shiny gold" ss1 `shouldBe` 126
            let ss2 =["light red bags contain 1 bright white bag, 2 muted yellow bags."
                     ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                     ,"bright white bags contain 1 shiny gold bag."
                     ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                     ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                     ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                     ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                     ,"faded blue bags contain no other bags."
                     ,"dotted black bags contain no other bags."]
            containedBags "shiny gold" ss2 `shouldBe` 32
    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day07input.txt"
        containedBags "shiny gold" p `shouldBe` 6006
