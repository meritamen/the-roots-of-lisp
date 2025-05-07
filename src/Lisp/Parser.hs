module Lisp.Parser (
  parseLispExpr
  , parseLisp
  , parseLispFile
  ) where

import Control.Exception hiding (try)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Lisp.Lexer
import Lisp.Types

lispSymbolP :: Parser LispVal
lispSymbolP = do
  text <- identifier
  case text of
    "t"   -> return LispT
    "nil" -> return LispNil
    sym   -> return $ LispSymbol sym

lispListP :: Parser LispVal
lispListP = LispList <$> parens (lispValP `sepEndBy` space)

lispDottedListP :: Parser LispVal
lispDottedListP
  = parens $ liftM2 LispDottedList (lispValP `sepEndBy` space) (dot *> lispValP)

lispQuoteP :: Parser LispVal
lispQuoteP = liftM (\x -> LispList [LispSymbol "quote", x]) (quote *> lispValP)

lispValP :: Parser LispVal
lispValP = choice $
  [lispQuoteP, lispSymbolP, try lispDottedListP, lispListP]

content :: Parser p -> Parser p
content p = space *> p <* eof

parseLispExpr :: Text -> LispVal
parseLispExpr input = case runParser (content lispValP) "<stdin>" input of
  Left err     -> throw err
  Right result -> result

parseLisp :: Text -> [LispVal]
parseLisp input = case runParser (content $ many lispValP) "<stdin>" input of
  Left err     -> throw err
  Right result -> result

parseLispFile :: String -> IO [LispVal]
parseLispFile = fmap parseLisp . TIO.readFile
