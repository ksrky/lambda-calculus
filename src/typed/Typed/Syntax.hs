module Typed.Syntax where

import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs String Ty Term
        | TmTrue
        | TmFalse
        | TmIf Term Term Term
        deriving (Show)

data Ty
        = TyBool
        | TyArr Ty Ty
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Ty
        deriving (Show)

data Command
        = Bind String Binding
        | Eval Term
        deriving (Show)

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
type Context = [(String, Binding)]

emptyContext :: Context
emptyContext = []

addBinding :: String -> Binding -> Context -> Context
addBinding x bind ctx = (x, bind) : ctx

addName :: String -> Context -> Context
addName x ctx = (x, NameBind) : ctx

pickFreshname :: String -> Context -> (String, Context)
pickFreshname x ctx = case lookup x ctx of
        Just _ -> pickFreshname (x ++ "'") ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ ctx !! i

getTypeFromContext :: MonadFail m => Context -> Int -> m Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> return tyT
        _ -> fail $ "Wrong kind of binding for variable " ++ index2name ctx i

getVarIndex :: MonadFail m => String -> Context -> m Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> return i
        Nothing -> fail $ "Unbound variable name: '" ++ var ++ "'"

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmAbs x tyT1 t2 -> TmAbs x tyT1 (walk (c + 1) t2)
                TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
                t -> t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c x n ->
                        if x < c
                                then TmVar x (n + d)
                                else TmVar (x + d) (n + d)
                )

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                ( \j x n ->
                        if x == j
                                then termShift j s
                                else TmVar x n
                )
                j

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

----------------------------------------------------------------
-- Printing
----------------------------------------------------------------
printtm :: Context -> Term -> String
printtm ctx t = case t of
        TmVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
        TmAbs x tyT1 t2 ->
                let (x', ctx') = pickFreshname x ctx
                 in "(λ" ++ x' ++ ": " ++ printty tyT1 ++ ". " ++ printtm ctx' t2 ++ ")"
        TmTrue -> "true"
        TmFalse -> "false"
        TmIf t1 t2 t3 -> "if " ++ printtm ctx t1 ++ " then " ++ printtm ctx t2 ++ " else " ++ printtm ctx t3

printty :: Ty -> String
printty ty = case ty of
        TyBool -> "Bool"
        TyArr tyT1 tyT2 -> "(" ++ printty tyT1 ++ " -> " ++ printty tyT2 ++ ")"