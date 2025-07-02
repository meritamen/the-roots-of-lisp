module Lisp.Types (
  LispVal (..)
  , EnvCtx
  , Eval (..)
  , LispError (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text  as T
import TextShow

data LispVal
  = LispSymbol Text
  | LispList [LispVal]
  | LispDottedList [LispVal] LispVal
  | LispT
  | LispNil
  | LispPrimitive ([LispVal] -> Eval LispVal)
  | LispFn { params :: [Text], body :: [LispVal], closure :: EnvCtx }

instance Eq LispVal where
  LispSymbol sym0 == LispSymbol sym1         = sym0 == sym1
  LispT == LispT                             = True
  LispNil == LispNil                         = True
  LispNil == LispList []                     = True
  LispList l == LispList r                   = l == r
  LispDottedList l lt == LispDottedList r rt = l == r && lt == rt
  _ == _                                     = False

instance Show LispVal where
  show = \case
    LispSymbol sym -> T.unpack sym
    LispList [] -> "()"
    LispList list -> "(" <> unwords (show <$> list) <> ")"
    LispDottedList list e -> "(" <> unwords (show <$> list) <> " . " <> show e <> ")"
    LispT -> "t"
    LispNil -> "()"
    LispPrimitive _ -> "#<primitive>"
    LispFn _ _ _ -> "#<lambda>"

instance TextShow LispVal where showb = fromString . show

type EnvCtx = IORef (Map Text (IORef LispVal))

newtype Eval a = Eval { unEval :: ExceptT LispError (ReaderT EnvCtx IO) a }
  deriving (Functor
           , Applicative
           , Monad
           , MonadError LispError
           , MonadReader EnvCtx
           , MonadIO)

data LispError
  = LispErrorWrongArgNumber Int [LispVal]
  | LispErrorTypeMismatch Text LispVal
  | LispErrorBadSpecialForm Text LispVal
  | LispErrorUnboundVar Text Text
  deriving Eq

instance TextShow LispError where
  showb = \case
    LispErrorWrongArgNumber i args -> "Expected " <> showb i
                                      <> " args; found values " <> showb args
    LispErrorTypeMismatch expected found ->
      "Invalid type: expected " <> showb expected <> ", found " <> showb found
    LispErrorBadSpecialForm msg form -> fromText msg <> ": " <> showb form
    LispErrorUnboundVar msg var -> fromText msg <> ": " <> fromText var

instance Show LispError where show = T.unpack . showt
