-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- to detect and run all specs
-- uncomment option, remove specific code below
import Test.Hspec
import Day20aSpec
import Day20bSpec
main = hspec $ do
    speca
    specb
