module Lisp.Types (
  LispVal (..)
  , Prettyprint (..)
  , EnvCtx
  , Eval (..)
  , LispError (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T

data LispVal
  = LispSymbol Text
  | LispList [LispVal]
  | LispDottedList [LispVal] LispVal
  | LispT
  | LispNil
  | LispPrimitive ([LispVal] -> Eval LispVal)
  | LispFn { params :: [Text], body :: [LispVal], closure :: EnvCtx }

instance Eq LispVal where
  LispSymbol sym0 == LispSymbol sym1 = sym0 == sym1
  LispT == LispT = True
  LispNil == LispNil = True
  LispNil == LispList [] = True
  LispList l == LispList r = l == r
  LispDottedList l lt == LispDottedList r rt = l == r && lt == rt
  _ == _ = False

class Prettyprint a where
  pp :: a -> Text

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

instance Prettyprint LispVal where pp = T.pack . show

type EnvCtx = IORef (Map Text (IORef LispVal))

newtype Eval a = Eval { unEval :: ExceptT LispError (ReaderT EnvCtx IO) a }
  deriving ( Functor
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

instance Show LispError where
  show = \case
    LispErrorWrongArgNumber i args -> "Expected " <> show i <> " args; found values " <> show args
    LispErrorTypeMismatch expected found -> "Invalid type: expected " <> show expected <> ", found " <> show found
    LispErrorBadSpecialForm msg form -> T.unpack msg <> ": " <> show form
    LispErrorUnboundVar msg var -> T.unpack msg <> ": " <> T.unpack var

instance Prettyprint LispError where pp = T.pack . show
