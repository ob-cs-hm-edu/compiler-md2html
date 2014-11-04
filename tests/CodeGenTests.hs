module CodeGenTests where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           CodeGen
import           IR

testEmptyAST :: Test.Framework.Test
testEmptyAST = testCase "generate Code for the empty AST"
    $ assertEqual "empty AST" (htmlHead ++ htmlFooter)
    $ generateHTML $ Sequence []

testText :: Test.Framework.Test
testText = testCase "generate Code for a text"
    $ assertEqual "text" "der Text"
    $ generateHTML' $ Text "der Text"

testP :: Test.Framework.Test
testP = testCase "generate Code for a paragraph"
    $ assertEqual "paragraph" "<p>der Text</p>\n"
    $ generateHTML' $ P [Text "der Text"]

testP' :: Test.Framework.Test
testP' = testCase "generate Code for a paragraph"
    $ assertEqual "paragraph" "<p>der Text</p>\n"
    $ generateHTML' $ P [Text "der", Text " ", Text "Text"]

testHI :: Test.Framework.Test
testHI = testCase "generate Code for a header"
    $ assertEqual "header" "<h1>der Header</h1>\n"
    $ generateHTML' $ H 1 (Sequence [Text "der", Text " ", Text "Header"])

testSequence :: Test.Framework.Test
testSequence = testCase "generate Code for a sequence"
    $ assertEqual "header"
      "<h1>der Header</h1>\n<p>Hallo</p>\n<p>Welt!</p>\n"
    $ generateHTML'
    $ Sequence [ H 1 (Sequence [Text "der", Text " ", Text "Header"])
               , P [Text "Hallo"]
               , P [Text "Welt!"]
               ]

-- testSequence = testCase "generate Code for a Sequence"

codeGenTests :: [Test.Framework.Test]
codeGenTests =
  [  testGroup "CodeGen Tests"
    [ testEmptyAST
    , testText
    , testP
    , testP'
    , testHI
    , testSequence
    ]
  ]


