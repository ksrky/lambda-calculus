{-# LANGUAGE OverloadedStrings #-}

module FOmega.Parser where

import FOmega.Syntax (
        Binding (TmAbbBind, TyAbbBind, TyVarBind, VarBind),
        Command (..),
        Context,
        Kind (..),
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
                        [ try $ pTmTApp ctx
                        , try $ pTmAbs ctx
                        , pTmTAbs ctx
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
        let ctx' = addName x ctx
        t2 <- pTerm ctx'
        return $ TmAbs x tyT1 t2

pTmTAbs :: Context -> Parser Term
pTmTAbs ctx = do
        _ <- symbol "\\"
        tyX <- lexeme pUCID
        _ <- symbol ":"
        knK1 <- lexeme pKind
        _ <- symbol "."
        let ctx' = addName tyX ctx
        t2 <- pTerm ctx'
        return $ TmTAbs tyX knK1 t2

pTmTApp :: Context -> Parser Term
pTmTApp ctx = TmTApp <$> lexeme (parens $ lexeme $ pTerm ctx) <*> between (symbol "[") (string "]") (lexeme $ pTy ctx)

pTy :: Context -> Parser Ty
pTy ctx =
        makeExprParser
                ( choice
                        [ try $ pTyAll ctx
                        , parens $ lexeme $ pTy ctx
                        , pTyVar ctx
                        ]
                )
                [ [InfixL (TyApp <$ symbol " ")]
                , [InfixR (TyArr <$ symbol "->")]
                ]
                <?> "`Type`"

pTyVar :: Context -> Parser Ty
pTyVar ctx = do
        x <- pUCID
        idx <- getVarIndex x ctx
        return $ TyVar idx (length ctx)

pTyAll :: Context -> Parser Ty
pTyAll ctx = do
        _ <- symbol "forall"
        x <- lexeme pUCID
        _ <- symbol "::"
        knK1 <- lexeme pKind
        _ <- symbol "."
        let ctx' = addName x ctx
        tyT2 <- pTy ctx'
        return $ TyAll x knK1 tyT2

pKind :: Parser Kind
pKind = makeExprParser (choice [KnStar <$ symbol "*", parens pKind]) [[InfixL (KnArr <$ symbol "->")]] <?> "`Kind`"

pCommand :: StateT Context Parser Command
pCommand =
        try
                ( do
                        x <- lift pLCID
                        ctx <- get
                        bind <- lift $ pBinder ctx
                        modify $ \ctx -> addName x ctx
                        return $ Bind x bind
                )
                <|> try
                        ( do
                                x <- lift pUCID
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
        try
                ( do
                        args <- pTyAbbArgs
                        _ <- symbol "="
                        tyT <- pTy ctx
                        let tyT' = foldr (uncurry TyAbs) tyT args
                        return $ TyAbbBind tyT' Nothing
                )
                <|> pure (TyVarBind KnStar)

pTyAbbArgs :: Parser [(String, Kind)]
pTyAbbArgs =
        try
                ( do
                        _ <- symbol "("
                        x <- pUCID
                        _ <- symbol ":"
                        k <- pKind
                        _ <- symbol ")"
                        rest <- pTyAbbArgs
                        return $ (x, k) : rest
                )
                <|> try
                        ( do
                                x <- pUCID
                                rest <- pTyAbbArgs
                                return $ (x, KnStar) : rest
                        )
                <|> pure []

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
