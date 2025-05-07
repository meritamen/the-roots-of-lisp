module Main (main)where

import Test.Tasty
import Test.Tasty.Hspec

import ParserSpec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [parserSpecs]
  defaultMain $ testGroup "All Tests" [testGroup "Specs" specs]
