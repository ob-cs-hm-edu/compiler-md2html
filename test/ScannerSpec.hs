module ScannerSpec (spec) where

import           Data.Maybe (fromJust)
import           Test.Hspec

import           Scanner
import           Types

scanAndExtractFirst :: String -> Token
scanAndExtractFirst = head . fromJust . scan

spec :: Spec
spec =
    describe "The Scanner" $ do

        it "scans the empty String" $
            scan "" `shouldBe` Just []

        it "scans a newline" $
            scanAndExtractFirst "\n" `shouldBe` TokenNewline

        it "scans a blank" $
            scanAndExtractFirst " h a l l o" `shouldBe` TokenBlanks 1

        it "scans 10 blanks" $
            scanAndExtractFirst "          h a l l o"
                `shouldBe` TokenBlanks 10

        it "scans H1" $
            scanAndExtractFirst "# Hallo" `shouldBe` TokenH 1

        it "scans H2" $
            scanAndExtractFirst "## Hallo" `shouldBe` TokenH 2

        it "scans H3" $
            scanAndExtractFirst "### Hallo" `shouldBe` TokenH 3

        it "scans H4" $
            scanAndExtractFirst "#### Hallo" `shouldBe` TokenH 4

        it "scans H5" $
            scanAndExtractFirst "##### Hallo" `shouldBe` TokenH 5

        it "scans H6" $
            scanAndExtractFirst "###### Hallo" `shouldBe` TokenH 6

        it "scans no H7 gut a Text" $
            scanAndExtractFirst "####### Hallo"
                `shouldBe` TokenText "#######"

        it "scans H4 without a following blank" $
            scanAndExtractFirst "####Hallo" `shouldBe` TokenH 4

        it "scans a string" $
            scanAndExtractFirst "Hallo" `shouldBe` TokenText "Hallo"
