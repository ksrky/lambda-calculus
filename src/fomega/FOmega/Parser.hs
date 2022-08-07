{-# LANGUAGE OverloadedStrings #-}

module FOmega.Parser where

import FOmega.Syntax (
        Binding (NameBind),
        Context,
        Kind (..),
        Term (..),
        Ty (TyAll, TyApp, TyArr, TyVar),
        addbinding,
        getVarIndex,
 )

import Control.Monad.Combinators.Expr (
        Operator (InfixL, InfixR, Postfix, Prefix),
        makeExprParser,
 )
import Control.Monad.State (
        MonadState (get, put),
        StateT,
        evalStateT,
 )
import Data.List (elemIndex)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (
        MonadParsec (eof, try),
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
        lowerChar,
        space1,
        string,
        upperChar,
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

pTmIdent :: Parser String
pTmIdent = (:) <$> lowerChar <*> many alphaNumChar <?> "`identifier`"

pTyIdent :: Parser String
pTyIdent = (:) <$> upperChar <*> many alphaNumChar <?> "`identifier`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensNosc :: Parser a -> Parser a
parensNosc = between (symbol "(") (string ")")

pTerm :: Parser Term
pTerm =
        choice
                [ try pTmTApp
                , try pTmAbs
                , pTmTAbs
                , try pTmApp
                , parensNosc pTerm
                , pTmVar
                ]
                <?> "`term`"
    where
        assocl :: Text -> (Term -> Term -> Term) -> Operator Parser Term
        assocl op f = InfixL (f <$ symbol op)

pTmNosc :: Parser Term
pTmNosc = choice [parensNosc $ lexeme pTerm, pTerm]

pTmApp :: Parser Term
pTmApp = TmApp <$> pTmNosc <* symbol " " <*> choice [parensNosc pTerm, pTmVar]

pTmAbs :: Parser Term
pTmAbs = do
        _ <- symbol "\\"
        x <- lexeme pTmIdent
        _ <- symbol ":"
        ty <- pTy
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        t1 <- pTerm
        put ctx
        return $ TmAbs x ty t1

pTmVar :: Parser Term
pTmVar = do
        x <- pTmIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TmVar idx (length ctx)

pTy :: Parser Ty
pTy =
        makeExprParser
                ( choice
                        [ parensNosc pTy
                        , pTyAll
                        , try pTyApp
                        , pTyVar
                        ]
                )
                [[assocr "->" TyArr]]
                <?> "`type`"
    where
        assocl :: Text -> (Ty -> Ty -> Ty) -> Operator Parser Ty
        assocl op f = InfixL (f <$ symbol op)
        assocr :: Text -> (Ty -> Ty -> Ty) -> Operator Parser Ty
        assocr op f = InfixR (f <$ symbol op)

pTyNosc :: Parser Ty
pTyNosc = choice [parensNosc pTy, pTyVar]

pTyApp :: Parser Ty
pTyApp = TyApp <$> pTyNosc <* symbol " " <*> pTy

pTyVar :: Parser Ty
pTyVar = do
        x <- pTyIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TyVar idx (length ctx)

pTyAll :: Parser Ty
pTyAll = do
        _ <- symbol "forall"
        x <- lexeme pTyIdent
        _ <- symbol "::"
        k <- lexeme pKind
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        ty <- pTy
        put ctx
        return $ TyAll x k ty

pTmTAbs :: Parser Term
pTmTAbs = do
        _ <- symbol "\\"
        x <- lexeme pTyIdent
        _ <- symbol "::"
        k <- lexeme pKind
        _ <- symbol "."
        ctx <- get
        addbinding x NameBind
        t1 <- pTerm
        put ctx
        return $ TmTAbs x k t1

pTmTApp :: Parser Term
pTmTApp = TmTApp <$> parens (lexeme pTerm) <*> between (symbol "[") (string "]") pTy

pKind :: Parser Kind
pKind = makeExprParser (choice [KnStar <$ symbol "*", parens pKind]) [[assocr "->" KnArr]] <?> "`kind`"
    where
        assocr :: Text -> (Kind -> Kind -> Kind) -> Operator Parser Kind
        assocr name f = InfixL (f <$ symbol name)

parseTerm :: String -> Either (ParseErrorBundle Text Void) Term
parseTerm input = parse ((lexeme pTerm `evalStateT` []) <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
