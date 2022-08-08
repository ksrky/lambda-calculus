{-# LANGUAGE OverloadedStrings #-}

module LambdaPi.Parser where

import LambdaPi.Syntax

import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = StateT Context (Parsec Void Text)

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

pLCID :: Parser String
pLCID = (:) <$> lowerChar <*> many alphaNumChar <?> "`LCID`"

pUCID :: Parser String
pUCID = (:) <$> upperChar <*> many alphaNumChar <?> "`UCID`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

pTerm :: Parser Term
pTerm =
        choice
                [pTmAbs, pTmAbs, try pTmApp, TmStar <$ string "*", pTmVar]
                <?> "`Term`"

pTmVar :: Parser Term
pTmVar = do
        x <- pID
        ctx <- get
        return $ TmVar (getVarIndex x ctx) (length ctx)

pTmAbs :: Parser Term
pTmAbs = do
        _ <- symbol "\\"
        x <- lexeme pID
        _ <- symbol ":"
        t1 <- pTerm
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        t2 <- pTerm
        put ctx
        return $ TmAbs x t1 t2

pTmApp :: Parser Term
pTmApp = TmApp <$> pTerm <* symbol " " <*> choice [parens pTerm, pTmVar]

pTmPi :: Parser Term
pTmPi = do
        _ <- symbol "foall"
        x <- lexeme pID
        _ <- symbol ":"
        t1 <- pTerm
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        t2 <- pTerm
        put ctx
        return $ TmPi x t1 t2

parseTerm :: String -> Either (ParseErrorBundle Text Void) Term
parseTerm input = parse ((lexeme pTerm `evalStateT` []) <* eof) "" (pack input)

parseProgram :: String -> Either (ParseErrorBundle Text Void) [Term]
parseProgram input = parse (evalStateT (pTerm `endBy` symbol ";") emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
