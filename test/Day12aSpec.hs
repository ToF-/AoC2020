module Day12aSpec
    where
import Test.Hspec
import Day12a

spec = do
    describe "movement" $ do
        describe "can be absolute" $ do
            it "to east" $ do
                let p = ((0,0),east)
                let q = move p east 10
                q `shouldBe` ((10,0),east)
            it "to west" $ do
                let p = ((0,0),east)
                let q = move p west 10
                q `shouldBe` ((-10,0),east)
            it "to south" $ do
                let p = ((0,0),east)
                let q = move p south 10
                q `shouldBe` ((0,10),east)
            it "to north" $ do
                let p = ((0,0),east)
                let q = move p north 10
                q `shouldBe` ((0,-10),east)
        describe "can be relative" $ do
            it "to east" $ do
                let p = ((0,0),east)
                let q = forward p 10
                q `shouldBe` ((10,0),east)
            it "to west" $ do
                let p = ((0,0),west)
                let q = forward p 10
                q `shouldBe` ((-10,0),west)
            it "to south" $ do
                let p = ((0,0),south)
                let q = forward p 10
                q `shouldBe` ((0,10),south)
            it "to north" $ do
                let p = ((0,0),north)
                let q = forward p 10
                q `shouldBe` ((0,-10),north)

    describe "rotate" $ do
        it "changes direction" $ do
            let p = ((0,0),north)
            let q = rotate p 90
            q `shouldBe` ((0,0),east)
            let p = ((0,0),east)
            let q = rotate p 90
            q `shouldBe` ((0,0),south)
            let p = ((0,0),south)
            let q = rotate p 90
            q `shouldBe` ((0,0),west)
            let p = ((0,0),west)
            let q = rotate p 90
            q `shouldBe` ((0,0),north)
            let p = ((0,0),west)
            let q = rotate p 180
            q `shouldBe` ((0,0),east)
            let p = ((0,0),north)
            let q = rotate p 270
            q `shouldBe` ((0,0),west)
            let p = ((0,0),north)
            let q = rotate p 360
            q `shouldBe` ((0,0),north)

    describe "interpreter" $ do
        describe "tells what movement to make" $ do
            it "relative" $ do
                let p = ((0,0),north)
                let q = interpret p "F10"
                q `shouldBe` ((0,-10),north)
                let p = ((0,0),east)
                let q = interpret p "F42"
                q `shouldBe` ((42,0),east)
            it "absolute" $ do
                let p = ((0,0),north)
                let q = interpret p "S3"
                q `shouldBe` ((0,3),north)
                let p = ((0,0),north)
                let q = interpret p "N3"
                q `shouldBe` ((0,-3),north)
                let p = ((0,0),north)
                let q = interpret p "E3"
                q `shouldBe` ((3,0),north)
                let p = ((0,0),north)
                let q = interpret p "W3"
                q `shouldBe` ((-3,0),north)
            it "rotation" $ do
                let p = ((0,0),north)
                let q = interpret p "R270"
                q `shouldBe` ((0,0),west)
                let p = ((0,0),north)
                let q = interpret p "L90"
                q `shouldBe` ((0,0),west)
    describe "travel" $ do
        it "execute several commands" $ do
            let commands = words "F10 N3 F7 R90 F11"
            let p = ((0,0),east)
            let q = travel p commands
            q `shouldBe` ((17,8),south)

    describe "distance" $ do
        it "tells the manhattan distance to origin" $ do
            distance (-10,5) `shouldBe` 15
            distance (50,5) `shouldBe` 55

    it "all of this should solve the puzzle" $ do
        commands <- fmap lines $ readFile "data/Day12input.txt"
        let p = ((0,0),east)
        let q = travel p commands
        distance (fst q) `shouldBe` 0
