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

pTmIdent :: Parser String
pTmIdent = (:) <$> lowerChar <*> many alphaNumChar <?> "`identifier`"

pTyIdent :: Parser String
pTyIdent = (:) <$> upperChar <*> many alphaNumChar <?> "`identifier`"

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

pTerm :: Parser Term
pTerm =
        makeExprParser
                ( choice
                        [ lexeme pTmAbs
                        , lexeme pTmTAbs
                        , parens pTerm
                        , pTmTApp
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
        x <- lexeme pTmIdent
        _ <- symbol ":"
        ty <- pTy
        _ <- symbol "."
        ctx <- get
        x' <- pickfreshname x
        t1 <- pTerm
        put ctx
        return $ TmAbs x' ty t1

pTmVar :: Parser Term
pTmVar = do
        x <- pTmIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TmVar idx (length ctx)

getVarIndex :: String -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: '" ++ var ++ "'"

pTy :: Parser Ty
pTy = makeExprParser (choice [pTyAll, pTySome, pTyVar]) [[assocr "->" TyArr]] <?> "`type`"
    where
        assocr :: Text -> (Ty -> Ty -> Ty) -> Operator Parser Ty
        assocr name f = InfixL (f <$ symbol name)

pTyVar :: Parser Ty
pTyVar = do
        x <- pTyIdent
        ctx <- get
        let idx = getVarIndex x ctx
        return $ TyVar idx (length ctx)

pTyAll :: Parser Ty
pTyAll = TyAll <$> (symbol "forall" *> pTyIdent) <*> pTy

pTySome :: Parser Ty
pTySome = TySome <$> (symbol "exists" *> pTyIdent) <*> pTy

pTmTAbs :: Parser Term
pTmTAbs = do
        _ <- symbol "\\"
        x <- lexeme pTyIdent
        _ <- symbol "."
        ctx <- get
        ctx' <- gets (execState (pickfreshname x))
        put ctx'
        t1 <- pTerm
        put ctx
        return $ TmTAbs x t1

pTmTApp :: Parser Term
pTmTApp = TmTApp <$> (pTerm <* symbol " ") <*> pTy

parseTerm :: String -> Either (ParseErrorBundle Text Void) Term
parseTerm input = parse ((pTerm `evalStateT` []) <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
