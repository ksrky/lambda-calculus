{
{-# LANGUAGE TupleSections #-}
module LambdaPi.Parser where

import LambdaPi.Lexer
import LambdaPi.Syntax
}

%name parse
%tokentype { Token }
%monad { IO } { >>= } { return }
%error { parseError }

%token

'='			{ TokEq }
'λ'                     { TokLambda }
'Π'                     { TokPi }
'.'			{ TokDot }
'->'			{ TokArrow }
':'			{ TokColon }
'*'                     { TokStar }
';'                     { TokSemi }
'('			{ TokLParen }
')'			{ TokRParen }

LCID             	{ TokLCID $$ }
UCID             	{ TokUCID $$ }

%%

-- Program :: { [Command] }
--         : Commands                                      {% $1 emptyContext }

Commands :: { Context -> IO [Command] }
        : Bind ';' Commands                             { \ctx -> do { (x, b) <- $1 ctx
                                                                ; let ctx' = addName x ctx
                                                                        in (Bind x b :) <#> $3 ctx' }  }
        | Term ';' Commands                             { \ctx -> do { cmd <- Eval <#> $1 ctx
                                                                ; (cmd :) <#> $3 ctx } } 
        | {- empty -}                                   { \_ -> return [] }

Bind    :: { Context -> IO (String, Binding) }
        : LCID ':' Type                                 { \ctx -> ($1,) <#> (VarBind <#> $3 ctx) }
        | LCID '=' Term                                 { \ctx -> ($1,) <#> (TmAbbBind <#> $3 ctx <*> pure Nothing) }
        | UCID ':' Kind                                 { \ctx -> ($1,) <#> (TyVarBind <#> $3 ctx) }
        | UCID '=' Type                                 { \ctx -> ($1,) <#> (TyAbbBind <#> $3 ctx <*> pure Nothing) }

Term	:: { Context -> IO Term }
	: 'λ' LCID ':' Type '.' Term   		        { \ctx -> let ctx' = addName $2 ctx 
                                                                in TmAbs $2 <#> $4 ctx <*> $6 ctx' }
	| Term2						{ $1 }

Term2	:: { Context -> IO Term }
	: Term2 Term1					{ \ctx -> TmApp <#> $1 ctx <*> $2 ctx }
	| Term1						{ $1 }

Term1	:: { Context -> IO Term }
	: LCID						{ \ctx -> do { idx <- getVarIndex $1 ctx
                                                                ; return $ TmVar idx (length ctx) } }
	| '(' Term ')'					{ $2 }

Type   :: { Context -> IO Type }
	: 'Π' LCID ':' Type '.' Type                    { \ctx -> let ctx' = addName $2 ctx in TyPi $2 <#> $4 ctx <*> $6 ctx' }
        | Type2 '->' Type				{ \ctx -> let ctx' = addName wc ctx in TyPi wc <#> $1 ctx <*> $3 ctx' }
        | Type2                                         { $1 }

Type2   :: { Context -> IO Type }
        : Type2 Term1                                   { \ctx -> TyApp <#> $1 ctx <*> $2 ctx }
        | Type1                                         { $1 }

Type1	:: { Context -> IO Type }
        : UCID						{ \ctx -> do { idx <- getVarIndex $1 ctx
                                                                ; return $ TyVar idx (length ctx) } }
	| '(' Type ')'					{ $2 }

Kind    :: { Context -> IO Kind }
        : 'Π' LCID ':' Type '.' Kind                    { \ctx -> let ctx' = addName $2 ctx in KnPi $2 <#> $4 ctx <*> $6 ctx' }
        | Type2 '->' Kind                               { \ctx -> let ctx' = addName wc ctx in KnPi wc <#> $1 ctx <*> $3 ctx' }
        | Kind1                                         { $1 }

Kind1   :: { Context -> IO Kind }
        : '*'                                           { \_ -> return KnStar }
	| '(' Kind ')'					{ $2 }

{
(<#>) :: Functor f => (a -> b) -> f a -> f b
(<#>) = (<$>)

infixl 4 <#>

wc :: String
wc = "_"

parseError :: [Token] -> IO a
parseError [] = fail "parse error at EOF"
parseError (t : _) = fail $ "parse error at " ++ show t
}