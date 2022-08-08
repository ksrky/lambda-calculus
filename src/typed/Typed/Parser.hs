{-# LANGUAGE OverloadedStrings #-}

module Typed.Parser where

import Control.Monad.Combinators.Expr (
        Operator (InfixL),
        makeExprParser,
 )
import Control.Monad.State (MonadState (get), evalStateT)
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
import Typed.Syntax (
        Binding (NameBind, VarBind),
        CT,
        Command (..),
        Term (..),
        Ty (..),
        addbinding,
        emptyContext,
        evalCT,
        getVarIndex,
 )

type Parser = CT (Parsec Void Text)

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
                        [ pTmAbs
                        , pTmIf
                        , TmTrue <$ string "true"
                        , TmFalse <$ string "false"
                        , parens $ lexeme pTerm
                        , pTmVar
                        ]
                )
                [[InfixL $ TmApp <$ symbol " "]]
                <?> "`term`"

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
        tyT1 <- lexeme pTy
        _ <- symbol "."
        addbinding x NameBind
        t2 <- pTerm
        return $ TmAbs x tyT1 t2

pTmIf :: Parser Term
pTmIf = TmIf <$> (symbol "if" *> lexeme pTerm) <*> (symbol "then" *> lexeme pTerm) <*> (symbol "else" *> pTerm)

pTy :: Parser Ty
pTy = makeExprParser (TyBool <$ string "Bool") [[InfixL $ TyArr <$ symbol "->"]] <?> "`Type`"

pCommand :: Parser Command
pCommand =
        try
                ( do
                        x <- pLCID
                        _ <- symbol ":"
                        ty <- pTy
                        addbinding x NameBind
                        return $ Bind x (VarBind ty)
                )
                <|> Eval <$> evalCT pTerm

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (lexeme pCommand `endBy` symbol ";") emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
