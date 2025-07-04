module EvalSpec where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Language.Lisp.Interpreter
import Language.Lisp.Parser
import Language.Lisp.Types

evalWithInitialEnv :: Text -> IO (Either LispError LispVal)
evalWithInitialEnv input = do
  envCtx <- initialEnvCtx
  runEval (eval . parseLispExpr $ input) envCtx

evalWithStd :: Text -> IO (Either LispError LispVal)
evalWithStd input = do
  envCtx <- initialEnvCtx
  void . flip runEval envCtx $ load "lispbits/stdlib.lisp" >>= liftM last . mapM eval
  runEval (eval . parseLispExpr $ input) envCtx

shouldEvalTo :: IO (Either LispError LispVal) -> Text -> Expectation
shouldEvalTo action expected = do
  result <- action
  result `shouldBe` Right (parseLispExpr expected)

test :: Text -> Text -> Spec
test expr expected = it (T.unpack expr) $ evalWithInitialEnv expr `shouldEvalTo` expected

testWithStd :: Text -> Text -> Spec
testWithStd expr expected = it (T.unpack expr) $ evalWithStd expr `shouldEvalTo` expected

spec_SevenPrimitiveOperaotrs :: Spec
spec_SevenPrimitiveOperaotrs =
  describe "Seven Primitive Operaotrs" $ do
  test "(quote a)" "a"
  test "'a" "a"
  test "(quote (a b c))" "(a b c)"
  test "(atom 'a)" "t"
  test "(atom '(a b c))" "()"
  test "(atom '())" "t"
  test "(atom (atom 'a))" "t"
  test "(atom '(atom 'a))" "()"
  test "(eq 'a 'a)" "t"
  test "(eq 'a 'b)" "()"
  test "(eq '() '())" "t"
  test "(car '(a b c))" "a"
  test "(cdr '(a b c))" "(b c)"
  test "(cons 'a '(b c))" "(a b c)"
  test "(cons 'a (cons 'b (cons 'c '())))" "(a b c)"
  test "(car (cons 'a '(b c)))" "a"
  test "(cdr (cons 'a '(b c)))" "(b c)"
  test "(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))" "second"

spec_DenotingFunctions :: Spec
spec_DenotingFunctions =
  describe "Denoting Functions" $ do
  test "((lambda (x) (cons x '(b))) 'a)" "(a b)"
  test "((lambda (x y) (cons x (cdr y))) 'z '(a b c))" "(z b c)"
  test "((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x)))" "(a b c)"
  test "((label subst (lambda (x y z) (cond ((atom z) (cond ((eq z y) x) ('t z))) ('t (cons (subst x y (car z)) (subst x y (cdr z))))))) 'm 'b '(a b (a b c) d))" "(a m (a m c) d)"
  test "((defun subst (x y z) (cond ((atom z) (cond ((eq z y) x) ('t z))) ('t (cons (subst x y (car z)) (subst x y (cdr z)))))) 'm 'b '(a b (a b c) d))" "(a m (a m c) d)"

spec_SomeFunctions :: Spec
spec_SomeFunctions =
  describe "Some Functions" $ do
  testWithStd "(cadr '((a b) (c d) e))" "(c d)"
  testWithStd "(caddr '((a b) (c d) e))" "e"
  testWithStd "(cdar '((a b) (c d) e))" "(b)"
  testWithStd "(cons 'a (cons 'b (cons 'c '())))" "(a b c)"
  testWithStd "(list 'a 'b 'c)" "(a b c)"
  testWithStd "(null 'a)" "()"
  testWithStd "(null '())" "t"
  testWithStd "(and (atom 'a) (eq 'a 'a))" "t"
  testWithStd "(and (atom 'a) (eq 'a 'b))" "()"
  testWithStd "(not (eq 'a 'a))" "()"
  testWithStd "(not (eq 'a 'b))" "t"
  testWithStd "(append '(a b) '(c d))" "(a b c d)"
  testWithStd "(append '() '(c d))" "(c d)"
  testWithStd "(pair '(x y z) '(a b c))" "((x a) (y b) (z c))"
  testWithStd "(assoc 'x '((x a) (y b)))" "a"
  testWithStd "(assoc 'x '((x new) (y b)))" "new"

spec_TheSurprise :: Spec
spec_TheSurprise =
  describe "The Surprise" $ do
  testWithStd "(eval 'x '((x a) (y b)))" "a"
  testWithStd "(eval '(eq 'a 'a) '())" "t"
  testWithStd "(eval '(cons x '(b c)) '((x a) (y b)))" "(a b c)"
  testWithStd "(eval '(cond ((atom x) 'atom) ('t 'list)) '((x '(a b))))" "list"
  testWithStd "(eval '(f '(b c)) '((f (lambda (x) (cons 'a x)))))" "(a b c)"
  testWithStd "(eval '((label firstatom (lambda (x) (cond ((atom x) x) ('t (firstatom (car x)))))) y) '((y ((a b) (c d)))))" "a"
  testWithStd "(eval '((lambda (x y) (cons x (cdr y))) 'a '(b c d)) '())" "(a c d)"

evalSpecs :: Spec
evalSpecs = describe "Eval" $ do
  spec_SevenPrimitiveOperaotrs
  spec_DenotingFunctions
  spec_SomeFunctions
  spec_TheSurprise
