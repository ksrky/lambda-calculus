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

pID :: Parser String
pID = (:) <$> letterChar <*> many alphaNumChar <?> "`ID`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

pTerm :: Context -> Parser Term
pTerm ctx =
        makeExprParser
                ( choice
                        [ try $ pTmAbs ctx
                        , parens $ lexeme $ pTerm ctx
                        , pTmVar ctx
                        ]
                )
                [[InfixL $ TmApp <$ symbol " "]]
                <?> "`Term`"

pTmVar :: Context -> Parser Term
pTmVar ctx = do
        x <- pID
        idx <- getVarIndex x ctx
        return $ TmVar idx (length ctx)

pTmAbs :: Context -> Parser Term
pTmAbs ctx = do
        _ <- symbol "\\"
        x <- lexeme pID
        _ <- symbol ":"
        tyT1 <- pTy ctx
        _ <- symbol "."
        let ctx' = addname x ctx
        t2 <- pTerm ctx'
        return $ TmAbs x tyT1 t2

pTmPi :: Context -> Parser Term
pTmPi ctx = do
        _ <- symbol "foall"
        x <- lexeme pID
        _ <- symbol ":"
        t1 <- pTy ctx
        _ <- symbol "."
        let ctx' = addname x ctx
        t2 <- pTy ctx'
        return $ TmPi x t1 t2

pTy :: Context -> Parser Ty
pTy ctx =
        choice
                [ TyStar <$ string "*"
                , TyTerm <$> pTerm ctx
                ]

pCommand :: StateT Context Parser Command
pCommand =
        try
                ( do
                        x <- lift pID
                        ctx <- get
                        bind <- lift $ pBinder ctx
                        modify $ \ctx -> addname x ctx
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

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
