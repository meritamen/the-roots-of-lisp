module Language.Lisp.Interpreter (
  runEval
  , initialEnvCtx
  , eval
  , load
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import TextShow

import Language.Lisp.Parser
import Language.Lisp.Types

runEval :: Eval a -> EnvCtx -> IO (Either LispError a)
runEval evaluation = runReaderT (runExceptT . unEval $ evaluation)

initialEnvCtx :: IO EnvCtx
initialEnvCtx
  = newIORef M.empty >>= flip bindVars (M.toList (LispPrimitive <$> primitives))

eval :: LispVal -> Eval LispVal
eval LispT         = return LispT
eval LispNil       = return LispNil
eval (LispList []) = return LispNil

eval (LispList (LispSymbol "cond":clauses)) = do
  case listToMaybe clauses of
    Nothing                          -> return LispNil
    Just (LispList [testForm, form]) -> do
      result <- eval testForm
      if result == LispNil
        then eval $ LispList (LispSymbol "cond":tail clauses)
        else eval form
    Just badForm                      -> throwError $
      LispErrorBadSpecialForm "Unrecognized special form" badForm

eval (LispSymbol varname) = getVar varname

eval (LispList [LispSymbol "quote", (LispList (LispSymbol "lambda":params:body))]) = do
  envCtxRef <- ask
  params' <- handleParams params
  mkFn envCtxRef params' body

eval (LispList [LispSymbol "quote", val]) = return val

eval (LispList [LispSymbol "label", LispSymbol sym, form]) = eval form >>= defineVar sym

eval (LispList (LispSymbol "defun":LispSymbol fnName:params:body)) = do
  envCtxRef <- ask
  params' <- handleParams params
  fn <- mkFn envCtxRef params' body
  defineVar fnName fn

eval (LispList (LispSymbol "lambda":params:body)) = do
  envCtxRef <- ask
  params' <- handleParams params
  mkFn envCtxRef params' body

eval (LispList (fn:args)) = do
  fn' <- eval fn
  argVals <- mapM eval args
  apply fn' argVals

eval badForm = throwError $ LispErrorBadSpecialForm "Unrecognized special form" badForm

mkFn :: EnvCtx -> [LispVal] -> [LispVal] -> Eval LispVal
mkFn envCtx params body = return $ LispFn (showt <$> params) body envCtx

handleParams :: LispVal -> Eval [LispVal]
handleParams LispNil = return mempty
handleParams (LispList params) = return params
handleParams badarg = throwError $ LispErrorTypeMismatch "cons" badarg

apply :: LispVal -> [LispVal] -> Eval LispVal
apply (LispPrimitive fn) args = fn args
apply (LispFn params body closure) args = do
  if length params /= length args
    then throwError $ LispErrorWrongArgNumber (length params) args
    else do
    envCtx' <- liftIO $ bindVars closure (zip params args)
    local (const envCtx') $ liftM last $ mapM eval body
apply badarg _ = throwError $ LispErrorTypeMismatch "function" badarg

isBound :: EnvCtx -> Text -> IO Bool
isBound envCtxRef varname = readIORef envCtxRef >>= return . M.member varname

getVar :: Text -> Eval LispVal
getVar varname = do
  envCtxRef <- ask
  envCtx <- liftIO . readIORef $ envCtxRef
  case M.lookup varname envCtx of
    Just result -> liftIO . readIORef $ result
    Nothing     -> throwError $
      LispErrorUnboundVar "Getting an unbound variable" varname

setVar :: Text -> LispVal -> Eval LispVal
setVar varname value = do
  envCtxRef <- ask
  envCtx <- liftIO . readIORef $ envCtxRef
  case M.lookup varname envCtx of
    Just result -> liftIO $ writeIORef result value
    Nothing     -> throwError $
      LispErrorUnboundVar "Setting an unbound variable" varname
  return value

defineVar :: Text -> LispVal -> Eval LispVal
defineVar varname value = do
  envCtxRef <- ask
  isDefined <- liftIO $ isBound envCtxRef varname
  if isDefined
    then setVar varname value >> return value
    else liftIO $ do
    valueRef <- newIORef value
    envCtx <- readIORef envCtxRef
    writeIORef envCtxRef $ M.insert varname valueRef envCtx
    return value

bindVars :: EnvCtx -> [(Text, LispVal)] -> IO EnvCtx
bindVars envCtxRef bindings
  = readIORef envCtxRef >>= extendEnvCtx bindings >>= newIORef
  where
    extendEnvCtx bindings envCtx
      = liftM (M.union envCtx) $ M.fromList <$> mapM addBinding bindings
    addBinding (varname, value) = newIORef value >>= return . (varname,)

primitives :: Map Text ([LispVal] -> Eval LispVal)
primitives = M.fromList $
  [ ("atom", atom), ("eq", eq), ("car", car), ("cdr", cdr), ("cons", cons)
  , ("list", list)]

atom :: [LispVal] -> Eval LispVal
atom [LispT]        = return LispT
atom [LispNil]      = return LispT
atom [LispList []]  = return LispT
atom [LispSymbol _] = return LispT
atom [_]            = return LispNil
atom badArgList     = throwError $ LispErrorWrongArgNumber 1 badArgList

eq :: [LispVal] -> Eval LispVal
eq [LispSymbol l, LispSymbol r] = return $ if l == r then LispT else LispNil
eq [LispT, LispT]               = return LispT
eq [LispNil , LispNil]          = return LispT
eq [LispNil, LispList []]       = return LispT
eq [LispList [], LispNil]       = return LispT
eq [LispList [], LispList []]   = return LispT
eq [_, _]                       = return LispNil
eq badArgList                   = throwError $
                                   LispErrorWrongArgNumber 2 badArgList

car :: [LispVal] -> Eval LispVal
car [LispNil]                = return LispNil
car [LispList []]            = return LispNil
car [LispList (x:_)]         = return x
car [LispDottedList (x:_) _] = return x
car [badArg]                 = throwError $ LispErrorTypeMismatch "cons" badArg
car badArgList               = throwError $ LispErrorWrongArgNumber 1 badArgList

cdr :: [LispVal] -> Eval LispVal
cdr [LispNil]                 = return LispNil
cdr [LispList []]             = return LispNil
cdr [LispList (_:xs)]         = return . LispList $ xs
cdr [LispDottedList [_] x]    = return x
cdr [LispDottedList (_:xs) x] = return $ LispDottedList xs x
cdr [badArg]                  = throwError $ LispErrorTypeMismatch "cons" badArg
cdr badArgList                = throwError $ LispErrorWrongArgNumber 1 badArgList

cons :: [LispVal] -> Eval LispVal
cons [x, LispList []]             = return $ LispList [x]
cons [x, LispList xs]             = return . LispList $ x:xs
cons [x, LispNil]                 = return $ LispList [x]
cons [x, LispDottedList xs xlast] = return $ LispDottedList (x:xs) xlast
cons [x1, x2]                     = return $ LispDottedList [x1] x2
cons badArgList                   = throwError $ LispErrorWrongArgNumber 2 badArgList

list :: [LispVal] -> Eval LispVal
list [] = return LispNil
list xs = return $ LispList xs

load :: String -> Eval [LispVal]
load fileName = liftIO $ parseLisp <$> TIO.readFile fileName
