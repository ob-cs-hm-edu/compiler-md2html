module CodeGenTests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import IR
import CodeGen

test_EmptyAST :: Test.Framework.Test
test_EmptyAST = testCase "generate Code for the empty AST"
    $ assertEqual "empty AST" (htmlHead ++ htmlFooter)
    $ generateHTML $ Sequence []

codeGenTests :: [Test.Framework.Test]
codeGenTests =
  [  testGroup "CodeGen Tests"
    [ test_EmptyAST
    ]
  ]


