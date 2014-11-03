module ParserTests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import IR
import Parser

test_EmptyTokenstream :: Test.Framework.Test
test_EmptyTokenstream = testCase "parse empty Tokenstream"
    $ assertEqual "empty Tokenstream" (Just $ Sequence [])
    $ parse []

parserTests :: [Test.Framework.Test]
parserTests =
  [  testGroup "Parser Tests"
    [ test_EmptyTokenstream
    ]
  ]

