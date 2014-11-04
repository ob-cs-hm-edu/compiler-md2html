module ScannerTests where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     as HUnit

import           Data.Maybe                     (fromJust)

import           IR
import           Scanner

scanAndExtractFirst :: String -> Token
scanAndExtractFirst = head . fromJust . scan

testEmptyString :: Test.Framework.Test
testEmptyString = testCase "scan empty String"
    $ assertEqual "empty String" (Just [])
    $ scan ""

testNewline :: Test.Framework.Test
testNewline = testCase "scan newline"
    $ assertEqual "newline" T_Newline
    $ scanAndExtractFirst "\n"

testBlank :: Test.Framework.Test
testBlank = testCase "scan blank"
    $ assertEqual "blank" (T_Blanks 1)
    $ scanAndExtractFirst " h a l l o"

testBlanks :: Test.Framework.Test
testBlanks = testCase "scan blanks"
    $ assertEqual "blanks" (T_Blanks 10)
    $ scanAndExtractFirst "          h a l l o"

testH1 :: Test.Framework.Test
testH1 = testCase "scan H1"
    $ assertEqual "H1" (T_H 1)
    $ scanAndExtractFirst "# Hallo"

testH2 :: Test.Framework.Test
testH2 = testCase "scan H2"
    $ assertEqual "H2" (T_H 2)
    $ scanAndExtractFirst "## Hallo"

testH3 :: Test.Framework.Test
testH3 = testCase "scan H3"
    $ assertEqual "H3" (T_H 3)
    $ scanAndExtractFirst "### Hallo"

testH4 :: Test.Framework.Test
testH4 = testCase "scan H4"
    $ assertEqual "H4" (T_H 4)
    $ scanAndExtractFirst "#### Hallo"

testH5 :: Test.Framework.Test
testH5 = testCase "scan H5"
    $ assertEqual "H5" (T_H 5)
    $ scanAndExtractFirst "##### Hallo"

testH6 :: Test.Framework.Test
testH6 = testCase "scan H6"
    $ assertEqual "H6" (T_H 6)
    $ scanAndExtractFirst "###### Hallo"

testH7 :: Test.Framework.Test
testH7 = testCase "scan H7"
    $ assertEqual "H7" (T_Text "#######")
    $ scanAndExtractFirst "####### Hallo"

testAHeader :: Test.Framework.Test
testAHeader = testCase "scan aHeader"
    $ assertEqual "aHeader" (T_H 4)
    $ scanAndExtractFirst "####Hallo"

testString :: Test.Framework.Test
testString = testCase "scan a string"
    $ assertEqual "s string" (T_Text "Hallo")
    $ scanAndExtractFirst "Hallo hallo"

scannerTests :: [Test.Framework.Test]
scannerTests =
  [  testGroup "Scanner Tests"
    [ testEmptyString
    , testNewline
    , testBlank
    , testBlanks
    , testH1
    , testH2
    , testH3
    , testH4
    , testH5
    , testH6
    , testH7
    , testAHeader
    , testString
    ]
  ]
