module ParserSpec where

import Test.Hspec

import Language.Lisp.Parser
import Language.Lisp.Types

spec_parseLispSymbol :: Spec
spec_parseLispSymbol =
  describe "parseLispSymbol" $ do
    it "parse defun" $
      parseLispExpr "defun" `shouldBe` LispSymbol "defun"
    it "parse defmacro" $
      parseLispExpr "defmacro" `shouldBe` LispSymbol "defmacro"
    it "parse meritamen/helm-display-buffer-regexp" $
      parseLispExpr "meritamen/helm-display-buffer-regexp"
        `shouldBe` LispSymbol "meritamen/helm-display-buffer-regexp"
    it "parse meritamen//display-helm-window" $
      parseLispExpr "meritamen//display-helm-window"
        `shouldBe` LispSymbol "meritamen//display-helm-window"
    it "parse t" $
      parseLispExpr "t" `shouldBe` LispT
    it "parse nil" $
      parseLispExpr "nil" `shouldBe` LispNil

spec_parseLispList :: Spec
spec_parseLispList =
  describe "parseLispList" $ do
    it "parse ()" $
      parseLispExpr "()" `shouldBe` LispList []
    it "parse (a)" $
      parseLispExpr "(a)" `shouldBe` LispList [LispSymbol "a"]
    it "parse (a b c d e f)" $
      parseLispExpr "(a b c d e f)"
        `shouldBe` LispList [LispSymbol "a", LispSymbol "b", LispSymbol "c", LispSymbol "d", LispSymbol "e", LispSymbol "f"]
    it "parse (t nil t nil)" $
      parseLispExpr "(t nil t nil)"
        `shouldBe` LispList [LispT, LispNil, LispT, LispNil]

spec_parseLispDottedList :: Spec
spec_parseLispDottedList =
  describe "parseLispDottedList" $ do
    it "parse (a . b)" $
      parseLispExpr "(a . b)" `shouldBe` LispDottedList [LispSymbol "a"] (LispSymbol "b")
    it "parse (a b . c)" $
      parseLispExpr "(a b . c)" `shouldBe` LispDottedList [LispSymbol "a", LispSymbol "b"] (LispSymbol "c")
    it "parse (t t . t)" $
      parseLispExpr "(t t . t)" `shouldBe` LispDottedList [LispT, LispT] LispT
    it "parse (nil nil . nil)" $
      parseLispExpr "(nil nil . nil)" `shouldBe` LispDottedList [LispNil, LispNil] LispNil
    it "parse (() () () . ())" $
      parseLispExpr "(() () () . ())" `shouldBe` LispDottedList [LispList [], LispList [], LispList []] (LispList [])
    it "parse ((((()))) ((())) (()) . ((())))" $
      parseLispExpr "((((()))) ((())) (()) . ((())))" `shouldBe`
        LispDottedList [LispList [LispList [LispList [LispList []]]]
                       , LispList [LispList [LispList []]]
                       , LispList [LispList []]] (LispList [LispList [LispList []]])

spec_parseQuote :: Spec
spec_parseQuote =
  describe "parseQuote" $ do
    it "parse 'a" $ parseLispExpr "'a" `shouldBe` LispList [LispSymbol "quote", LispSymbol "a"]
    it "parse 't" $ parseLispExpr "'t" `shouldBe` LispList [LispSymbol "quote", LispT]
    it "parse '()" $ parseLispExpr "'()" `shouldBe` LispList [LispSymbol "quote", LispList []]
    it "parse '''''()" $
      parseLispExpr "'''''()" `shouldBe`
        LispList [LispSymbol "quote",
                  LispList [LispSymbol "quote",
                            LispList [LispSymbol "quote",
                                      LispList [LispSymbol "quote",
                                                LispList [LispSymbol "quote",
                                                          LispList []]]]]]
    it "parse '('('(''() . ('''a))))" $
      parseLispExpr "'('('(''() . ('''a))))" `shouldBe`
      LispList [LispSymbol "quote",
                LispList [
                   LispList [LispSymbol "quote",
                             LispList[
                                LispList [LispSymbol "quote",
                                          LispDottedList [LispList [LispSymbol "quote",
                                                                    LispList [LispSymbol "quote",
                                                                              LispList []]]] (LispList [LispList [LispSymbol "quote",
                                                                                                                   LispList [LispSymbol "quote",
                                                                                                                             LispList [LispSymbol "quote",
                                                                                                                                        LispSymbol "a"]]]])]]]]]

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
  spec_parseLispSymbol
  spec_parseLispList
  spec_parseLispDottedList
  spec_parseQuote
