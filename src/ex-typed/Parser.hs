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
                        [ pTmLet ctx
                        , pTmFix ctx
                        , pTmRecord ctx
                        , pTmCase ctx
                        , pTmTag ctx
                        , try pTmUnit
                        , try $ pTmTuple ctx
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

pTmLet :: Context -> Parser Term
pTmLet ctx = do
        symbol "let"
        symbol "{"
        x <- lexeme pLCID
        symbol "="
        t1 <- lexeme $ pTerm ctx
        symbol ";"
        symbol "}"
        symbol "in"
        let ctx' = addname x ctx
        t2 <- pTerm ctx'
        return $ TmLet x t1 t2

pTmFix :: Context -> Parser Term
pTmFix ctx = do
        symbol "fix"
        t1 <- pTerm ctx
        return $ TmFix t1

pTmTuple :: Context -> Parser Term
pTmTuple ctx = do
        symbol "("
        fields <- pTerm ctx `sepBy` symbol ","
        string ")"
        let labs = map show [1 ..]
        return $ TmRecord (zip labs fields)

pTmRecord :: Context -> Parser Term
pTmRecord ctx = do
        symbol "{"
        fields <- pRecordField `sepBy` symbol ","
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
        symbol "of"
        symbol "{"
        (li, xi) <- lexeme pPat
        symbol "->"
        let ctx' = addname xi ctx
        t1 <- lexeme $ pTerm ctx'
        alts <- pAlts ctx'
        string "}"
        return $ TmCase t alts
    where
        pAlts :: Context -> Parser [(String, (String, Term))]
        pAlts ctx = do
                symbol "|"
                (li, xi) <- lexeme pPat
                symbol "->"
                let ctx' = addname xi ctx
                ti <- lexeme $ pTerm ctx'
                rest <- pAlts ctx'
                return $ (li, (xi, ti)) : rest

pPat :: Parser (String, String)
pPat = (,) <$> lexeme pLCID <*> pLCID

pTmTag :: Context -> Parser Term
pTmTag ctx = do
        symbol "<"
        l <- lexeme pLCID
        t1 <- lexeme $ pTerm ctx
        symbol ":"
        tyT2 <- lexeme $ pTy ctx
        string ">"
        return $ TmTag l t1 tyT2

pTmUnit :: Parser Term
pTmUnit = TmUnit <$ parens (string "")

pTy :: Context -> Parser Ty
pTy ctx =
        makeExprParser
                ( choice
                        [ pTyRecord ctx
                        , pTyVariant ctx
                        , try pTyUnit
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

pTyUnit :: Parser Ty
pTyUnit = TyUnit <$ parens (string "")

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
                                bind <- lift $ lexeme $ pTyBinder ctx
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

pTyBinder :: Context -> Parser Binding
pTyBinder ctx =
        TyAbbBind <$> (symbol "=" *> pTy ctx)
                <|> pure TyVarBind

pCommands :: String -> Either (ParseErrorBundle Text Void) [Command]
pCommands input = parse (evalStateT (pCommand `endBy` lift (symbol ";")) emptyContext <* eof) "" (pack input)

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty
