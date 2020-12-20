-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- to detect and run all specs
-- uncomment option, remove specific code below
import Test.Hspec
import Day20aSpec
main = hspec $ do
    spec
