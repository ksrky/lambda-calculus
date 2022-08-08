{-# LANGUAGE OverloadedStrings #-}

module SystemF.Parser where

import SystemF.Syntax (
        Binding (..),
        Command (..),
        Context,
        Term (..),
        Ty (..),
        addbinding,
        emptyContext,
        evalCT,
        getVarIndex,
 )

import Control.Monad.Combinators.Expr (
        Operator (InfixL, Postfix, Prefix),
        makeExprParser,
 )
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Data.List (elemIndex)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (
        MonadParsec (eof, try),
        ParseErrorBundle,
        Parsec,
        between,
        choice,
        endBy,
        errorBundlePretty,
        many,
        parse,
        (<?>),
        (<|>),
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

pLCID :: Parser String
pLCID = (:) <$> lowerChar <*> many alphaNumChar <?> "`LCID`"

pUCID :: Parser String
pUCID = (:) <$> upperChar <*> many alphaNumChar <?> "`UCID`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

pTerm :: Parser Term
pTerm =
        makeExprParser
                ( choice
                        [ try pTmTApp
                        , try pTmAbs
                        , pTmTAbs
                        , parens $ lexeme pTerm
                        , pTmVar
                        ]
                )
                [[InfixL $ TmApp <$ symbol " "]]
                <?> "`Term`"

pTmVar :: Parser Term
pTmVar = do
        x <- pLCID
        ctx <- get
        idx <- getVarIndex x ctx
        return $ TmVar idx (length ctx)

pTmAbs :: Parser Term
pTmAbs = do
        _ <- symbol "\\"
        x <- lexeme pLCID
        _ <- symbol ":"
        tyT1 <- evalCT $ lexeme pTy
        _ <- symbol "."
        addbinding x NameBind
        t2 <- pTerm
        return $ TmAbs x tyT1 t2

pTmTAbs :: Parser Term
pTmTAbs = do
        _ <- symbol "\\"
        tyX <- lexeme pUCID
        _ <- symbol "."
        addbinding tyX NameBind
        t1 <- pTerm
        return $ TmTAbs tyX t1

pTmTApp :: Parser Term
pTmTApp = TmTApp <$> lexeme (evalCT $ parens $ lexeme pTerm) <*> between (symbol "[") (string "]") (lexeme pTy)

pTy :: Parser Ty
pTy = makeExprParser (choice [try pTyAll, pTyVar]) [[InfixL $ TyArr <$ symbol "->"]] <?> "`Type`"

pTyVar :: Parser Ty
pTyVar = do
        x <- pUCID
        ctx <- get
        idx <- getVarIndex x ctx
        return $ TyVar idx (length ctx)

pTyAll :: Parser Ty
pTyAll = do
        _ <- symbol "forall"
        x <- lexeme pUCID
        _ <- symbol "."
        addbinding x NameBind
        ty <- pTy
        return $ TyAll x ty

pCommand :: Parser Command
pCommand =
        try
                ( do
                        x <- pLCID
                        bind <- pBinder
                        addbinding x NameBind
                        return $ Bind x bind
                )
                <|> try
                        ( do
                                x <- pUCID
                                bind <- pTyBinder
                                addbinding x NameBind
                                return $ Bind x bind
                        )
                <|> Eval <$> evalCT pTerm

pBinder :: Parser Binding
pBinder =
        VarBind <$> (symbol ":" *> pTy)
                <|> TmAbbBind <$> (symbol "=" *> pTerm) <*> pure Nothing

pTyBinder :: Parser Binding
pTyBinder =
        TyAbbBind <$> (symbol "=" *> pTy)
                <|> pure TyVarBind

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (lexeme pCommand `endBy` symbol ";") emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty