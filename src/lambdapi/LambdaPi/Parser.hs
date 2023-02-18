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

pLCID :: Parser String
pLCID = (:) <$> lowerChar <*> many alphaNumChar <?> "`LCID`"

pUCID :: Parser String
pUCID = (:) <$> upperChar <*> many alphaNumChar <?> "`UCID`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

pTerm :: Context -> Parser Term
pTerm ctx =
        makeExprParser
                ( choice
                        [ pTmAbs ctx
                        , parens $ lexeme $ pTerm ctx
                        , pTmVar ctx
                        ]
                )
                [[InfixL $ TmApp <$ symbol " "]]
                <?> "`Term`"

pTmVar :: Context -> Parser Term
pTmVar ctx = do
        x <- pLCID
        idx <- getVarIndex x ctx
        return $ TmVar idx (length ctx)

pTmAbs :: Context -> Parser Term
pTmAbs ctx = do
        _ <- symbol "\\"
        x <- lexeme pLCID
        _ <- symbol ":"
        tyT1 <- pTy ctx
        _ <- symbol "."
        let ctx' = addName x ctx
        t2 <- pTerm ctx'
        return $ TmAbs x tyT1 t2

pTy :: Context -> Parser Ty
pTy ctx =
        choice
                [ try $ pTyApp ctx
                , try $ pTyPi ctx
                , parens $ lexeme $ pTy ctx
                , pTyVar ctx
                ]
                <?> "`Type`"

pTyVar :: Context -> Parser Ty
pTyVar ctx = do
        tyX <- pLCID
        idx <- getVarIndex tyX ctx
        return $ TyVar idx (length ctx)

pTyApp :: Context -> Parser Ty
pTyApp ctx = do
        tyT1 <- pTy ctx
        t2 <- pTerm ctx
        return $ TyApp tyT1 t2

pTyPi :: Context -> Parser Ty
pTyPi ctx = do
        _ <- symbol "Π"
        x <- lexeme pLCID
        _ <- symbol ":"
        tyT1 <- pTy ctx
        _ <- symbol "."
        let ctx' = addName x ctx
        tyT2 <- pTy ctx'
        return $ TyPi x tyT1 tyT2

pKind :: Context -> Parser Kind
pKind ctx =
        choice
                [ KnStar <$ symbol "*"
                , pKnPi ctx
                , parens $ lexeme $ pKind ctx
                ]
                <?> "`Kind`"

pKnPi :: Context -> Parser Kind
pKnPi ctx = do
        _ <- symbol "Π"
        x <- lexeme pLCID
        _ <- symbol ":"
        tyT1 <- pTy ctx
        _ <- symbol "."
        let ctx' = addName x ctx
        knK2 <- pKind ctx'
        return $ KnPi x tyT1 knK2

pCommand :: StateT Context Parser Command
pCommand =
        try
                ( do
                        x <- lift $ lexeme pLCID
                        ctx <- get
                        bind <- lift $ pBinder ctx
                        modify $ \ctx -> addName x ctx
                        return $ Bind x bind
                )
                <|> try
                        ( do
                                x <- lift $ lexeme pUCID
                                ctx <- get
                                bind <- lift $ pTyBinder ctx
                                modify $ \ctx -> addName x ctx
                                return $ Bind x bind
                        )
                <|> ( do
                        ctx <- get
                        t <- lift $ pTerm ctx
                        return $ Eval t
                    )

pBinder :: Context -> Parser Binding
pBinder ctx =
        VarBind <$> (symbol ":" *> pTy ctx)
                <|> TmAbbBind <$> (symbol "=" *> pTerm ctx) <*> pure Nothing

pTyBinder :: Context -> Parser Binding
pTyBinder ctx =
        TyVarBind <$> (symbol ":" *> pKind ctx)
                <|> TyAbbBind <$> (symbol "=" *> pTy ctx) <*> pure Nothing

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
