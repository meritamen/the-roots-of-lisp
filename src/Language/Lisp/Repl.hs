module Language.Lisp.Repl (
  runRepl
  ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import TextShow

import Language.Lisp.Interpreter
import Language.Lisp.Parser
import Language.Lisp.Types

evalText :: EnvCtx -> Text -> IO Text
evalText envCtxRef text = do
  result <- runEval (eval . parseLispExpr $ text) envCtxRef
  case result of
    Left err     -> return . showt $ err
    Right result -> return . showt $ result

evalAndPrint :: EnvCtx -> Text -> IO ()
evalAndPrint envCtxRef text = evalText envCtxRef text >>= TIO.putStrLn

runRepl :: Maybe String -> IO ()
runRepl fileName = do
  envCtx <- initialEnvCtx
  case fileName of
    Nothing -> runInputT defaultSettings . loop . evalAndPrint $ envCtx
    Just fileName -> do
      void . flip runEval envCtx $ load fileName >>= liftM last . mapM eval
      runInputT defaultSettings . loop . evalAndPrint $ envCtx
  where
    loop action = do
      minput <- getInputLine "* "
      case minput of
        Nothing -> outputStrLn "Goodbye~"
        Just input -> (liftIO . action . T.pack $ input) >> loop action
