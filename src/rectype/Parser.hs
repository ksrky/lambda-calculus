{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr (
        Operator (InfixL, Postfix, Prefix),
        makeExprParser,
 )
import Control.Monad.State
import Data.Text (Text, pack)
import Data.Void (Void)
import Syntax
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
                        [ try $ pTmRecord ctx
                        , pTmCase ctx
                        , pTmTag ctx
                        , try $ pTmAbs ctx
                        , try $ parens $ lexeme $ pTerm ctx
                        , pTmVar ctx
                        ]
                )
                [[InfixL $ try $ TmApp <$ symbol " "]]
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

pTmRecord :: Context -> Parser Term
pTmRecord ctx = do
        symbol "{"
        fields <- try (pRecordField `sepBy` symbol ",")
        string "}"
        return $ TmRecord fields
    where
        pRecordField :: Parser (String, Term)
        pRecordField = do
                li <- lexeme pLCID
                symbol ":"
                ti <- lexeme $ pTerm ctx
                return (li, ti)

pTmProj :: Context -> Parser Term
pTmProj ctx = do
        t1 <- pTerm ctx
        string "."
        TmProj t1 <$> pLCID

pTmCase :: Context -> Parser Term
pTmCase ctx = do
        symbol "case"
        t <- lexeme $ pTerm ctx
        symbol ";"
        symbol "{"
        alts <- pAlt ctx `sepBy` symbol "|"
        string "}"
        return $ TmCase t alts
    where
        pAlt :: Context -> Parser (String, (String, Term))
        pAlt ctx = do
                (li, xi) <- lexeme pPat
                symbol "->"
                let ctx' = addname xi ctx
                ti <- lexeme $ pTerm ctx'
                symbol ";"
                return (li, (xi, ti))

pPat :: Parser (String, String)
pPat = (,) <$> lexeme pLCID <*> pLCID

pTmTag :: Context -> Parser Term
pTmTag ctx = do
        symbol "<"
        l <- lexeme pLCID
        symbol "="
        t1 <- lexeme $ pTerm ctx
        symbol ">"
        symbol "as"
        tyT2 <- pTy ctx
        return $ TmTag l t1 tyT2

pTy :: Context -> Parser Ty
pTy ctx = do
        makeExprParser
                ( choice
                        [ pTyRecord ctx
                        , pTyVariant ctx
                        , parens $ lexeme $ pTy ctx
                        , pTyVar ctx
                        ]
                )
                [[InfixL $ TyArr <$ symbol "->"]]
                <?> "`Type`"

pTyVar :: Context -> Parser Ty
pTyVar ctx = do
        x <- pUCID
        idx <- getVarIndex x ctx
        return $ TyVar idx (length ctx)

pTyRecord :: Context -> Parser Ty
pTyRecord ctx = do
        symbol "{"
        fields <- pRecordField `sepBy` symbol ","
        string "}"
        return $ TyRecord fields
    where
        pRecordField :: Parser (String, Ty)
        pRecordField = do
                li <- lexeme pLCID
                symbol ":"
                tyTi <- lexeme $ pTy ctx
                return (li, tyTi)

pTyVariant :: Context -> Parser Ty
pTyVariant ctx = do
        symbol "<"
        fields <- pVariantFields `sepBy` symbol "|"
        string ">"
        return $ TyVariant fields
    where
        pVariantFields :: Parser (String, Ty)
        pVariantFields = do
                li <- pLCID
                symbol ":"
                tyTi <- lexeme $ pTy ctx
                return (li, tyTi)

pCommand :: StateT Context Parser Command
pCommand =
        try
                ( do
                        x <- lift $ lexeme pLCID
                        ctx <- get
                        bind <- lift $ lexeme $ pBinder ctx
                        modify $ \ctx -> addname x ctx
                        return $ Bind x bind
                )
                <|> try
                        ( do
                                x <- lift $ lexeme pUCID
                                ctx <- get
                                bind <- lift $ lexeme $ pTyBinder ctx x
                                modify $ \ctx -> addname x ctx
                                return $ Bind x bind
                        )
                <|> ( do
                        ctx <- get
                        t <- lift $ lexeme $ pTerm ctx
                        return $ Eval t
                    )

pBinder :: Context -> Parser Binding
pBinder ctx =
        VarBind <$> (symbol ":" *> pTy ctx)
                <|> TmAbbBind <$> (symbol "=" *> pTerm ctx) <*> pure Nothing

pTyBinder :: Context -> String -> Parser Binding
pTyBinder ctx x =
        try
                ( TyAbbBind
                        <$> ( symbol "=" *> do
                                let ctx' = addname x ctx
                                TyRec x <$> pTy ctx'
                            )
                )
                <|> pure TyVarBind

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
