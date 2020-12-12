module Day12bSpec
    where
import Test.Hspec
import Day12b

spec = do
    describe "movement" $ do
        describe "can be on the waypoint" $ do
            it "to north" $ do
                let p = ((0,0),(10,-11))
                let q = move p north 10
                waypoint q `shouldBe` (10,-21)
            it "to west" $ do
                let p = ((0,0),(10,-11))
                let q = move p west 10
                waypoint q `shouldBe` (0,-11)
            it "to south" $ do
                let p = ((0,0),(10,-11))
                let q = move p south 10
                waypoint q `shouldBe` (10,-1)
            it "to east" $ do
                let p = ((0,0),(10,-11))
                let q = move p east 10
                waypoint q `shouldBe` (20,-11)

        describe "can be relative" $ do
            it "to the waypoint" $ do
                let p = ((0,0),(10,1))
                let q = forward p 10
                position q `shouldBe` (100,10)

                let p = ((0,0),(-10,1))
                let q = forward p 10
                position q `shouldBe` (-100,10)

                let p = ((0,0),(-10,-1))
                let q = forward p 10
                position q `shouldBe` (-100,-10)

    describe "rotate" $ do
       it "rotate the viewpoint" $ do
           let p = ((0,0),(10,-4))
           let q = rotate p 90
           waypoint q `shouldBe` (4,10)
           let p = ((0,0),(4,10))
           let q = rotate p 90
           waypoint q `shouldBe` (-10,4)
           let p = ((0,0),(10,-4))
           let q = rotate p 270
           waypoint q `shouldBe` (-4,-10)
           let p = ((0,0),(10,-4))
           let q = rotate p 180
           waypoint q `shouldBe` (-10,4)

    describe "interpreter" $ do
       describe "tells what movement to make" $ do
           it "waypoint" $ do
               let p = ((0,0),(10,4))
               let q = interpret p "N10"
               waypoint q `shouldBe` (10,-6)
           it "forward" $ do
               let p = ((0,0),(10,4))
               let q = interpret p "F10"
               position q `shouldBe` (100,40)
           it "rotate" $ do
               let p = ((0,0),(10,4))
               let q = interpret p "R90"
               waypoint q `shouldBe` (-4,10)

    describe "travel" $ do
        it "execute several commands" $ do
            let commands = words "F10 N3 F7 R90 F11"
            let p = ((0,0),(10,-1))
            let q = travel p commands
            position q `shouldBe` (214,72)

    describe "distance" $ do
        it "tells the manhattan distance to origin" $ do
            distance (-10,5) `shouldBe` 15
            distance (50,5) `shouldBe` 55

    it "all of this should solve the puzzle" $ do
        commands <- fmap lines $ readFile "data/Day12input.txt"
        let p = ((0,0),(10,-1))
        let q = travel p commands
        distance (fst q) `shouldBe` 52069
