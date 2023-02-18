{
module LambdaPi.Lexer where
}

%wrapper "basic"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [a-zA-Z]

@lcid = $lower [$alpha $digit \_ \']*
@ucid = $upper [$alpha $digit \_ \']*

tokens :-

$white+                 ;

\=                      { \_ -> TokEq }
\λ                      { \_ -> TokLambda }
\Π                      { \_ -> TokPi }
\.                      { \_ -> TokDot }
\-\>                    { \_ -> TokArrow }
\:                      { \_ -> TokColon }
\*                      { \_ -> TokStar }
\;                      { \_ -> TokSemi }
\(                      { \_ -> TokLParen }
\)                      { \_ -> TokRParen }

@lcid                  { \s -> TokLCID s }
@ucid                  { \s -> TokUCID s }

{
data Token
    = TokEq
    | TokLambda
    | TokPi
    | TokDot
    | TokArrow
    | TokColon
    | TokStar
    | TokSemi
    | TokLParen
    | TokRParen
    | TokLCID String
    | TokUCID String
    deriving (Eq, Show)
}