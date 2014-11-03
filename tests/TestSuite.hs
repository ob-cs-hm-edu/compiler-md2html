module Main where

import Test.Framework

import ScannerTests
import ParserTests
import CodeGenTests

tests :: [Test]
tests = scannerTests ++ parserTests ++ codeGenTests

main :: IO ()
main = defaultMain tests

