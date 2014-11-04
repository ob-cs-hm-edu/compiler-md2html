module Main where

import           Test.Framework

import           CodeGenTests
import           ParserTests
import           ScannerTests

tests :: [Test]
tests = scannerTests ++ parserTests ++ codeGenTests

main :: IO ()
main = defaultMain tests

