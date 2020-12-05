module Day04bSpec
    where
import Test.Hspec
import Day04b
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

            it "if it has a number" $ do
                field "byr:foo" `shouldBe` Nothing

            it "if it has a 4 digits" $ do
                field "byr:0001970" `shouldBe` Nothing

            it "if it is within 1920 and 2002" $ do
                field "byr:1919" `shouldBe` Nothing 
                field "byr:2005" `shouldBe` Nothing 


        describe "can be a cid" $ do
            it "if it has a cid key" $ do 
                field "cid:XX37" `shouldBe` Just (CID,"XX37") 
                field "cid:XX65" `shouldBe` Just (CID,"XX65") 
                field "foo:XX80" `shouldBe` Nothing 

        describe "can be a ecl" $ do
            it "if it has a ecl key" $ do 
                field "ecl:oth" `shouldBe` Just (ECL,"oth")

            it "if it is one of the colors" $ do
                field "ecl:amb" `shouldBe` Just (ECL,"amb")
                field "ecl:blu" `shouldBe` Just (ECL,"blu")
                field "ecl:brn" `shouldBe` Just (ECL,"brn")
                field "ecl:gry" `shouldBe` Just (ECL,"gry")
                field "ecl:grn" `shouldBe` Just (ECL,"grn")
                field "ecl:hzl" `shouldBe` Just (ECL,"hzl")
                field "ecl:oth" `shouldBe` Just (ECL,"oth")
                field "ecl:foo" `shouldBe` Nothing 

        describe "can be a eyr" $ do
            it "if it has a eyr key" $ do 
                field "eyr:2030" `shouldBe` Just (EYR,"2030")

            it "if it has a number" $ do
                field "eyr:foo" `shouldBe` Nothing

            it "if it has a 4 digits" $ do
                field "eyr:0001970" `shouldBe` Nothing

            it "if it is within 2020 and 2030" $ do
                field "eyr:2019" `shouldBe` Nothing 
                field "eyr:2031" `shouldBe` Nothing 


        describe "can be a hcl" $ do
            it "if it has a hcl key" $ do 
                field "hcl:#abcdef" `shouldBe` Just (HCL,"#abcdef")
            it "if it has a sharp and 6 digits" $ do
                field "hcl:abcdef"  `shouldBe` Nothing 

            it "if it has 6 hex lowercase digits" $ do
                field "hcl:#foofoo"  `shouldBe` Nothing 
                field "hcl:#abc0abc"  `shouldBe` Nothing 
                field "hcl:#01BCD3"  `shouldBe` Nothing 

        describe "can be a hgt" $ do
            it "if it has a hgt key" $ do 
                field "hgt:173cm" `shouldBe` Just (HGT,"173cm")

            it "if it has a unit of cm or inch" $ do
                   field "hgt:150" `shouldBe` Nothing  

            it "if it is within limits for a given unit" $ do
                   field "hgt:149cm" `shouldBe` Nothing 
                   field "hgt:194cm" `shouldBe` Nothing 
                   field "hgt:58in" `shouldBe` Nothing 
                   field "hgt:77in" `shouldBe` Nothing 

        describe "can be a iyr" $ do
            it "if it has a iyr key" $ do 
                field "iyr:2015" `shouldBe` Just (IYR,"2015")

            it "if it has a 4 digits" $ do
                field "iyr:0002015" `shouldBe` Nothing

            it "if it is within 2010 and 2020" $ do
                field "iyr:2009" `shouldBe` Nothing 
                field "iyr:2021" `shouldBe` Nothing 

        describe "can be a pid" $ do
            it "if it has a pid key" $ do 
                field "pid:123456789" `shouldBe` Just (PID,"123456789")
            it "if it has 9 digits" $ do
                field "pid:12345678" `shouldBe` Nothing 
                field "pid:12345@7890" `shouldBe` Nothing 

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
            validCount  p`shouldBe`  114

