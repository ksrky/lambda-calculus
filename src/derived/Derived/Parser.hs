{-# LANGUAGE OverloadedStrings #-}

module Derived.Parser where

import Control.Monad.Combinators.Expr (
    Operator (InfixL, Postfix, Prefix),
    makeExprParser,
 )
import Control.Monad.State
import Data.Text (Text, pack)
import Data.Void (Void)
import Derived.Syntax
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
            [ TmTrue <$ string "true"
            , TmFalse <$ string "false"
            , TmZero <$ string "zero"
            , pTmSucc ctx
            , try $ pTmAbs ctx
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
    tyT1 <- lexeme $ pTy ctx
    _ <- symbol "."
    let ctx' = addname x ctx
    t2 <- pTerm ctx'
    return $ TmAbs x tyT1 t2

pTmSucc :: Context -> Parser Term
pTmSucc ctx = do
    _ <- symbol "succ"
    t <- pTerm ctx
    return $ TmSucc t

pTy :: Context -> Parser Ty
pTy ctx = makeExprParser (choice [pTyVar ctx]) [[InfixL $ TyArr <$ symbol "->"]] <?> "`Type`"

pTyVar :: Context -> Parser Ty
pTyVar ctx = do
    x <- pUCID
    idx <- getVarIndex x ctx
    return $ TyVar idx (length ctx)

pCommand :: StateT Context Parser Command
pCommand =
    try
        ( do
            x <- lift pLCID
            ctx <- get
            bind <- lift $ pBinder ctx
            modify $ \ctx -> addname x ctx
            return $ Bind x bind
        )
        <|> try
            ( do
                x <- lift pUCID
                ctx <- get
                bind <- lift $ pTyBinder ctx
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

pTyBinder :: Context -> Parser Binding
pTyBinder ctx =
    TyAbbBind <$> (symbol "=" *> pTy ctx)
        <|> pure TyVarBind

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty