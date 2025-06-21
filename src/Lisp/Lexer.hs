module Lisp.Lexer (
  Parser
  , symbol
  , parens
  , dot
  , quote
  , doubleQuote
  , identifier
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

dot :: Parser ()
dot = void $ symbol "."

quote :: Parser ()
quote = void $ symbol "'"

doubleQuote :: Parser ()
doubleQuote = void $ symbol "\""

specialChar :: Parser Char
specialChar = oneOf ("!#$%&|*+-/:<=>?@^_~'" :: String)

identifier :: Parser Text
identifier = lexeme . try $
  fmap T.pack $ (:) <$> (letterChar <|> specialChar)
                    <*> (many (alphaNumChar <|> specialChar))
