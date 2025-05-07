module Main (main)where

import Test.Tasty
import Test.Tasty.Hspec

import EvalSpec
import ParserSpec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [parserSpecs, evalSpecs]
  defaultMain $ testGroup "All Tests" [testGroup "Specs" specs]
