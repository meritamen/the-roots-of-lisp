module EvalSpec where

import Control.Monad.IO.Class
import Data.Text (Text)
import Test.Hspec

import Lisp.Eval
import Lisp.Parser
import Lisp.Types

evalWithInitialEnv :: Text -> IO (Either LispError LispVal)
evalWithInitialEnv input = do
  envCtx <- initialEnvCtx
  runEval (eval . parseLispExpr $ input) envCtx

spec_evalQuotedSymbol :: Spec
spec_evalQuotedSymbol =
  describe "evalQuotedSymbol" $ do
    it "eval 'a" $ do
      result <- liftIO $ evalWithInitialEnv "'a"
      result `shouldSatisfy` not . isLispError
    it "eval a" $ do
      result <- liftIO $ evalWithInitialEnv "a"
      result `shouldSatisfy` isLispError

spec_evalApply :: Spec
spec_evalApply =
  describe "evalApply" $ do
    it "eval 'a" $ do
      result <- liftIO $ evalWithInitialEnv "((lambda (x) (car (car x))) '((a . b) c))"
      result `shouldSatisfy` not . isLispError

isLispError :: Either LispError LispVal -> Bool
isLispError (Left _) = True
isLispError (Right _) = False

evalSpecs :: Spec
evalSpecs = describe "Eval" $ do
  spec_evalQuotedSymbol
  spec_evalApply
