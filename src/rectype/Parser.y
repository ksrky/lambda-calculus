{
module Parser where

import Lexer
import Syntax
}

%name parse Commands
%tokentype { Token }
%monad { Alex }
%lexer { lexer } { TokEOF }
%error { parseError }

%token
    'as'            { TokKeyword KwAs }  
    'case'          { TokKeyword KwCase }
    'fold'          { TokKeyword KwFold }
    'in'            { TokKeyword KwIn }
    'of'            { TokKeyword KwOf }
    'let'           { TokKeyword KwLet }
    'unfold'        { TokKeyword KwUnfold }

    '->'            { TokSymbol SymArrow }
    '\\'            { TokSymbol SymBackslash }
    ':'             { TokSymbol SymColon }
    ','             { TokSymbol SymComma }
    '.'             { TokSymbol SymDot }
    '='             { TokSymbol SymEqual }
    '>'             { TokSymbol SymGT }
    '{'             { TokSymbol SymLBrace }
    '['             { TokSymbol SymLBrack }
    '('             { TokSymbol SymLParen }
    '<'             { TokSymbol SymLT }
    '}'             { TokSymbol SymRBrace }
    ']'             { TokSymbol SymRBrack }
    ')'             { TokSymbol SymRParen }
    ';'             { TokSymbol SymSemicolon }
    '|'             { TokSymbol SymVBar }

    LCID            { TokLCID $$ }
    UCID            { TokUCID $$ }

%%

Term        :: { Term }
            : '\\' LCID ':' Ty '.' Term                 {% do
                                                            modifyContext (addname $2)
                                                            return $ TmAbs $2 $4 $6
                                                        }
            | Term ATerm                                { TmApp $1 $2 }
            | '{' fields '}'                            { TmRecord $2 }
            | 'case' Term 'of' '{' alts '}'             { TmCase $2 $5 }
            | '<' LCID '=' Term '>' 'as' Ty             { TmTag $2 $4 $7 }
            | 'fold' '[' Ty ']' Term                    { TmApp (TmFold $3) $5 }
            | 'unfold' '[' Ty ']' Term                  { TmApp (TmUnfold $3) $5 }
            | ATerm                                     { $1 }

alts        :: { [(String, (String, Term))] }
            : LCID LCID '->' Term '|' alts              {% do
                                                            modifyContext (addname $2)
                                                            return $ ($1, ($2, $4)) : $6 }
            | LCID LCID '->' Term                       { [($1, ($2, $4))] }

ATerm       :: { Term }
            : LCID                                      {% do
                                                            ctx <- getContext
                                                            case getVarIndex $1 ctx of
                                                                Just i -> return $ TmVar i (length ctx)
                                                                Nothing -> alexError $ "Unbound variable name: '" ++ $1 ++ "'"
                                                        }
            | Term '.' LCID                             { TmProj $1 $3 }
            | '(' Term ')'                              { $2 }                              

fields      :: { [(String, Term)] }
            : LCID '=' Term ',' fields                  { ($1, $3) : $5 }
            | LCID '=' Term                             { [($1, $3)] }

Ty          :: { Ty }
            : '{' tyfields '}'                          { TyRecord $2 }
            | '<' varfields '>'                         { TyVariant $2 }

ATy         :: { Ty }
            : UCID                                      {% do
                                                            ctx <- getContext
                                                            case getVarIndex $1 ctx of
                                                                Just i -> return $ TyVar i (length ctx)
                                                                Nothing -> alexError $ "Unbound variable name: '" ++ $1 ++ "'"
                                                        }
            | '(' Ty ')'                                { $2 }

tyfields    :: { [(String, Ty)] }
            : UCID ':' Ty ',' tyfields                  { ($1, $3) : $5 }
            | UCID ':' Ty                               { [($1, $3)] }

varfields   :: { [(String, Ty)] }
            : UCID ':' Ty '|' varfields                 { ($1, $3) : $5 }
            | UCID ':' Ty                               { [($1, $3)] }

Command     :: { Command }
            : LCID ':' Ty                               { Bind $1 (VarBind $3) }
            | LCID '=' Term                             { Bind $1 (TmAbbBind $3 Nothing) }
            | UCID '=' Ty                               { Bind $1 (TyAbbBind $3) }
            | UCID                                      { Bind $1 TyVarBind }
            | Term                                      { Eval $1 }

Commands    :: { [Command] }
            : Command ';' Commands                      { $1 : $3 }
            | Command ';'                               { [$1] }
            | {- empty -}                               { [] }

{
parseError :: Token -> Alex a
parseError t = alexError $ "parse error: " ++ show t
}