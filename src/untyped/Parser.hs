{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Syntax

import Control.Monad.Combinators.Expr (
        Operator (InfixL, Postfix, Prefix),
        makeExprParser,
 )
import Control.Monad.State
import Data.List (elemIndex)
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

pIdent :: Parser String
pIdent = (:) <$> letterChar <*> many alphaNumChar <?> "`identifier`"

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

pTerm :: Parser Term
pTerm = makeExprParser (lexeme pTmAbs <|> parens pTerm <|> pTmVar) [[assocl " " TmApp]]
    where
        assocl :: Text -> (Term -> Term -> Term) -> Operator Parser Term
        assocl name f = InfixL (f <$ symbol name)

pTmAbs :: Parser Term
pTmAbs = do
        _ <- symbol "\\"
        x <- lexeme pIdent
        _ <- symbol "."
        ctx <- get
        ctx' <- gets (execState (pickfreshname x))
        put ctx'
        t1 <- pTerm
        put ctx
        return $ TmAbs x t1

pTmVar :: Parser Term
pTmVar = do
        x <- pIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TmVar idx (length ctx)

getVarIndex :: String -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: '" ++ var ++ "'"

parseTerm :: String -> Either (ParseErrorBundle Text Void) Term
parseTerm input = parse ((pTerm `evalStateT` []) <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
