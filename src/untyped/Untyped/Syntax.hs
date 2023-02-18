module Untyped.Syntax where

import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs String Term
        deriving (Eq, Show)

data Binding = NameBind deriving (Show)

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
                TmAbs x t2 -> TmAbs x (walk (c + 1) t2)

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

termSubst :: Term -> Int -> Term -> Term
termSubst s =
        tmmap
                ( \j x n ->
                        if x == j
                                then termShift j s
                                else TmVar x n
                )

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst (termShift 1 s) 0 t)

----------------------------------------------------------------
-- Printing
----------------------------------------------------------------
printtm :: Context -> Term -> String
printtm ctx t = case t of
        TmVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ printtm ctx t2 ++ ")"
        TmAbs x t1 ->
                let (x', ctx') = pickFreshname x ctx
                 in "(λ" ++ x' ++ ". " ++ printtm ctx' t1 ++ ")"
