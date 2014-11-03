module ScannerTests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Scanner

test_EmptyString :: Test.Framework.Test
test_EmptyString = testCase "scan empty String"
    $ assertEqual "empty String" (Just [])
    $ scan ""

scannerTests :: [Test.Framework.Test]
scannerTests =
  [  testGroup "Scanner Tests"
    [ test_EmptyString
    ]
  ]
