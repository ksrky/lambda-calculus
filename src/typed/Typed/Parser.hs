{-# LANGUAGE OverloadedStrings #-}

module Typed.Parser where

import Typed.Syntax (
        Binding (VarBind),
        Command (..),
        Context,
        Term (..),
        Ty (..),
        addName,
        emptyContext,
        getVarIndex,
 )

import Control.Monad.Combinators.Expr (
        Operator (InfixL, InfixR),
        makeExprParser,
 )
import Control.Monad.State (
        MonadState (get),
        MonadTrans (lift),
        StateT,
        evalStateT,
        modify,
 )
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
                        , pTmIf ctx
                        , TmTrue <$ string "true"
                        , TmFalse <$ string "false"
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
        tyT1 <- lexeme pTy
        _ <- symbol "."
        let ctx' = addName x ctx
        t2 <- pTerm ctx'
        return $ TmAbs x tyT1 t2

pTmIf :: Context -> Parser Term
pTmIf ctx = TmIf <$> (symbol "if" *> pTerm ctx) <*> (symbol "then" *> lexeme (pTerm ctx)) <*> (symbol "else" *> pTerm ctx)

pTy :: Parser Ty
pTy = makeExprParser (TyBool <$ string "Bool") [[InfixR $ TyArr <$ symbol "->"]] <?> "`Type`"

pCommand :: StateT Context Parser Command
pCommand =
        try
                ( do
                        x <- lift pLCID
                        _ <- lift $ symbol ":"
                        ty <- lift pTy
                        modify $ \ctx -> addName x ctx
                        return $ Bind x (VarBind ty)
                )
                <|> ( do
                        ctx <- get
                        t <- lift $ pTerm ctx
                        return $ Eval t
                    )

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
