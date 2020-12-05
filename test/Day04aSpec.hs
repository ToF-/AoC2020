module Day04aSpec
    where
import Test.Hspec
import Day04a
-- byr (Birth Year)
-- cid (Country ID)
-- ecl (Eye Color)
-- eyr (Expiration Year)
-- hcl (Hair Color)
-- hgt (Height)
-- iyr (Issue Year)
-- pid (Passport ID)
spec = do
    describe "collect" $ do
        it "collects groups of lines separated by an empty line" $ do
            let ls = ["hello"
                     ,"world"
                     ,""
                     ,"foo bar"
                     ,"qux"]
            collect ls `shouldBe`
                ["hello world"
                ,"foo bar qux"]

    describe "a field" $ do
        describe "can be a byr" $ do
            it "if it has a byr key" $ do 
                field "byr:1937" `shouldBe` Just (BYR,"1937") 
                field "byr:1965" `shouldBe` Just (BYR,"1965") 
                field "foo:1980" `shouldBe` Nothing 

        describe "can be a cid" $ do
            it "if it has a cid key" $ do 
                field "cid:XX37" `shouldBe` Just (CID,"XX37") 
                field "cid:XX65" `shouldBe` Just (CID,"XX65") 
                field "foo:XX80" `shouldBe` Nothing 

        describe "can be a ecl" $ do
            it "if it has a ecl key" $ do 
                field "ecl:oth" `shouldBe` Just (ECL,"oth")

        describe "can be a eyr" $ do
            it "if it has a eyr key" $ do 
                field "eyr:2030" `shouldBe` Just (EYR,"2030")

        describe "can be a hcl" $ do
            it "if it has a hcl key" $ do 
                field "hcl:#abcdef" `shouldBe` Just (HCL,"#abcdef")

        describe "can be a hgt" $ do
            it "if it has a hgt key" $ do 
                field "hgt:173cm" `shouldBe` Just (HGT,"173cm")

        describe "can be a iyr" $ do
            it "if it has a iyr key" $ do 
                field "iyr:2015" `shouldBe` Just (IYR,"2015")

        describe "can be a pid" $ do
            it "if it has a pid key" $ do 
                field "pid:123456789" `shouldBe` Just (PID,"123456789")
    describe "password is valid" $ do
        it "if it has all the keys" $ do
            let s = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd  byr:1937 iyr:2017 cid:147 hgt:183cm valid"
            valid s `shouldBe` True 
            let t = "ecl:gry eyr:2020 hcl:#fffffd  byr:1937 iyr:2017 cid:147 hgt:183cm valid"
            valid t `shouldBe` False

        it "if it's missing only cid" $ do
            let s = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd  byr:1937 iyr:2017 hgt:183cm valid"
            valid s `shouldBe` True

    describe "validCount" $ do
        it "counts the valid passwords" $ do
            let ls = ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                     ,"byr:1937 iyr:2017 cid:147 hgt:183cm"
                     ,""
                     ,"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                     ,"hcl:#cfa07d byr:1929"
                     ,""
                     ,"hcl:#ae17e1 iyr:2013"
                     ,"eyr:2024"
                     ,"ecl:brn pid:760753108 byr:1931"
                     ,"hgt:179cm"
                     ,""
                     ,"hcl:#cfa07d eyr:2025 pid:166559648"
                     ,"iyr:2011 ecl:brn hgt:59in"]
            validCount ls `shouldBe` 2 
    it "all of this should solve the puzzle" $ do
        p <- fmap lines $ readFile "./data/Day04input.txt"
        validCount  p`shouldBe`  196

