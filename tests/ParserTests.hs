module ParserTests where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           IR
import           Parser

fromSequence :: AST -> [AST]
fromSequence (Sequence asts) = asts
fromSequence _ = error "fromSequence"

testEmptyTokenstream :: Test.Framework.Test
testEmptyTokenstream = testCase "parse empty Tokenstream"
    $ assertEqual "empty Tokenstream" (Just $ Sequence [])
    $ parse []

testNewline :: Test.Framework.Test
testNewline = testCase "parse Newline"
    $ assertEqual "Newline" (Just $ Sequence [P [Text "\n"]])
    $ parse [T_Newline]

test2Newlines :: Test.Framework.Test
test2Newlines = testCase "parse 2 Newlines"
    $ assertEqual "2 Newlines" (Just $ Sequence [Emptyline])
    $ parse [T_Newline, T_Newline]

test2Texts :: Test.Framework.Test
test2Texts = testCase "parse 2 Texts"
    $ assertEqual "2 Texts" (Just $ Sequence [P [Text "Hallo", Text "\n"]])
    $ parse [T_Text "Hallo", T_Newline]

testNotAHeaderBecauseOfBlanks :: Test.Framework.Test
testNotAHeaderBecauseOfBlanks = testCase "parse not a header"
    $ assertEqual "not a Header"
        (Just $ Sequence [P [Text "######", Text "Hallo"]])
    $ parse [T_H 6, T_Text "Hallo"]

testAHeader :: Test.Framework.Test
testAHeader = testCase "parse a header"
    $ assertEqual "a Header"
        (Just $ Sequence [ H 6 (Sequence [Text "Hallo", Text " ", Text "Welt!"])
                         , P [Text "\n" ]])
    $ parse [T_H 6, T_Blanks 1, T_Text "Hallo", T_Blanks 1, T_Text "Welt!", T_Newline]

testAHeaderWithoutNewline :: Test.Framework.Test
testAHeaderWithoutNewline = testCase "parse a header without a newline"
    $ assertEqual "a Header without a newline"
        (Just $ Sequence [ H 6 (Sequence [Text "Hallo", Text " ", Text "Welt!"]) ])
    $ parse [T_H 6, T_Blanks 1, T_Text "Hallo", T_Blanks 1, T_Text "Welt!"]

testAHeaderWithEmptyline :: Test.Framework.Test
testAHeaderWithEmptyline = testCase "parse a header with two newlines"
    $ assertEqual "a Header with two newlines"
        (Just $ Sequence [ H 6 (Sequence [Text "Hallo", Text " ", Text "Welt!"])
                         , Emptyline])
    $ parse [T_H 6, T_Blanks 1, T_Text "Hallo", T_Blanks 1, T_Text "Welt!"
            ,T_Newline, T_Newline]

test2Paragraphs :: Test.Framework.Test
test2Paragraphs = testCase "parse 2 Paragraphs"
    $ assertEqual "2 Paragraphs"
      (Just $ Sequence [P [Text "Hallo", Text "\n", Text "Welt", Text "   "]
                       , Emptyline
                       ,P [Text "Huhu", Text " ", Text "World", Text "\n"]])
    $ parse [ T_Text "Hallo", T_Newline, T_Text "Welt", T_Blanks 3
            , T_Newline, T_Newline
            , T_Text "Huhu", T_Blanks 1, T_Text "World", T_Newline]

parserTests :: [Test.Framework.Test]
parserTests =
  [  testGroup "Parser Tests"
    [ testEmptyTokenstream
    , testNewline
    , test2Newlines
    , test2Texts
    , testNotAHeaderBecauseOfBlanks
    , testAHeader
    , testAHeaderWithoutNewline
    , testAHeaderWithEmptyline
    , test2Paragraphs
    ]
  ]

