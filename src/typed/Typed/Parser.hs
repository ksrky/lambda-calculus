{-# LANGUAGE OverloadedStrings #-}

module Typed.Parser where

import Typed.Syntax (
        Binding (NameBind),
        Context,
        Term (..),
        Ty (..),
        addbinding,
        getVarIndex,
 )

import Control.Monad.Combinators.Expr (
        Operator (InfixL, Postfix, Prefix),
        makeExprParser,
 )
import Control.Monad.State (
        MonadState (get, put),
        StateT,
        evalStateT,
 )
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (
        MonadParsec (eof),
        ParseErrorBundle,
        Parsec,
        between,
        choice,
        errorBundlePretty,
        many,
        parse,
        (<?>),
 )
import Text.Megaparsec.Char (
        alphaNumChar,
        letterChar,
        space1,
        string,
 )
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
parens = between (symbol "(") (string ")")

pTerm :: Parser Term
pTerm =
        makeExprParser
                ( choice
                        [ parens $ lexeme pTerm
                        , pTmAbs
                        , pTmIf
                        , TmTrue <$ symbol "true"
                        , TmFalse <$ symbol "false"
                        , pTmVar
                        ]
                )
                [[assocl " " TmApp]]
                <?> "`term`"
    where
        assocl :: Text -> (Term -> Term -> Term) -> Operator Parser Term
        assocl name f = InfixL (f <$ symbol name)

pTmAbs :: Parser Term
pTmAbs = do
        _ <- symbol "\\"
        x <- lexeme pIdent
        _ <- symbol ":"
        tyT1 <- lexeme pTy
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        t2 <- pTerm
        put ctx
        return $ TmAbs x tyT1 t2

pTy :: Parser Ty
pTy = makeExprParser (lexeme $ TyBool <$ string "Bool") [[assocr "->" TyArr]] <?> "`type`"
    where
        assocr :: Text -> (Ty -> Ty -> Ty) -> Operator Parser Ty
        assocr name f = InfixL (f <$ symbol name)

pTmVar :: Parser Term
pTmVar = do
        x <- pIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TmVar idx (length ctx)

pTmIf :: Parser Term
pTmIf = TmIf <$> (symbol "if" *> lexeme pTerm) <*> (symbol "then" *> lexeme pTerm) <*> (symbol "else" *> lexeme pTerm)

parseTerm :: String -> Either (ParseErrorBundle Text Void) Term
parseTerm input = parse ((lexeme pTerm `evalStateT` []) <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
