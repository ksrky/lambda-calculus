{
module Lexer where

import Syntax
}

%wrapper "monadUserState"

$small = a-z
$large = A-Z
$alpha = [a-zA-Z]
$digit = 0-9

@LCID = $small [$alpha $digit \_ \']*
@UCID = $large [$alpha $digit \_ \']*

tokens :-

<0> $white+                             ;
<0> "--" .*                             ;

<0> as                                  { keyword KwAs }
<0> case                                { keyword KwCase }
<0> in                                  { keyword KwIn }
<0> of                                  { keyword KwOf }
<0> let                                 { keyword KwLet }

<0> \-\>                                { symbol SymArrow }
<0> \\                                  { symbol SymBackslash }
<0> \:                                  { symbol SymColon }
<0> \,                                  { symbol SymComma }
<0> \.                                  { symbol SymDot }
<0> \=                                  { symbol SymEqual }
<0> \>                                  { symbol SymGT }
<0> \{                                  { symbol SymLBrace }
<0> \(                                  { symbol SymLParen }
<0> \<                                  { symbol SymLT }
<0> \}                                  { symbol SymRBrace }
<0> \)                                  { symbol SymRParen }
<0> \;                                  { symbol SymSemicolon }
<0> \|                                  { symbol SymVBar }

<0> @LCID                               { lcid }
<0> @UCID                               { ucid }

{
data Token
    = TokKeyword Keyword
    | TokSymbol Symbol
    | TokLCID String
    | TokUCID String
    | TokEOF
    deriving(Eq, Show)

data Keyword
    = KwAs | KwCase | KwIn | KwOf | KwLet | KwFold | KwUnfold
    deriving (Eq, Show)

data Symbol
    = SymArrow | SymBackslash | SymColon | SymComma | SymDot
    | SymEqual | SymGT | SymLBrace | SymLBrack | SymLParen | SymLT
    | SymRBrace | SymRBrack | SymRParen | SymSemicolon | SymVBar
    deriving (Eq, Show)

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

type Action = AlexAction Token

keyword :: Keyword -> Action
keyword key _ _ = return $ TokKeyword key

symbol :: Symbol -> Action
symbol sym _ _ = return $ TokSymbol sym

lcid :: Action
lcid = \(_, _, _, str) len -> return $ TokLCID (take len str)

ucid :: Action
ucid = \(_, _, _, str) len -> return $ TokUCID (take len str)

alexEOF :: Alex Token
alexEOF = return TokEOF

data AlexUserState = AlexUserState { context :: Context }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { context = [] }

getContext :: Alex Context
getContext = context <$> alexGetUserState

setContext :: Context -> Alex ()
setContext ctx = do
    ust <- alexGetUserState
    alexSetUserState ust{ context = ctx }

modifyContext :: (Context -> Context) -> Alex ()
modifyContext f = do
    ctx <- getContext
    setContext (f ctx)
}

