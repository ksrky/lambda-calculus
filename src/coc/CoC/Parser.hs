{-# LANGUAGE OverloadedStrings #-}

module CoC.Parser where

import CoC.Syntax

import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pID :: Parser String
pID = (:) <$> letterChar <*> many alphaNumChar <?> "`ID`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")
