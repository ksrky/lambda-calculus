{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typed.Syntax where

import Control.Monad.State (
        MonadState (get, state),
        State,
        StateT,
        modify,
        runState,
 )
import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmTrue
        | TmFalse
        | TmIf Term Term Term
        deriving (Show)

data Ty = TyArr Ty Ty | TyBool deriving (Eq, Show)

data Binding = NameBind | VarBind Ty

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
type Context = [(String, Binding)]

emptyContext :: Context
emptyContext = []

addbinding :: Monad m => String -> Binding -> StateT Context m ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

pickfreshname :: String -> Context -> (String, Context)
pickfreshname x ctx = case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

getVarIndex :: String -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: '" ++ var ++ "'"

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> tyT
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x tyT1 t2 -> TmAbs x tyT1 (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
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
        TmAbs x tyT1 t2 ->
                let (x', ctx') = pickfreshname x ctx
                 in "(λ" ++ x' ++ ": " ++ printty ctx tyT1 ++ ". " ++ printtm ctx' t2 ++ ")"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ printtm ctx t2 ++ ")"
        TmTrue -> "true"
        TmFalse -> "false"
        TmIf t1 t2 t3 -> "if " ++ printtm ctx t1 ++ " then " ++ printtm ctx t2 ++ " else " ++ printtm ctx t3

printty :: Context -> Ty -> String
printty ctx ty = case ty of
        TyBool -> "Bool"
        TyArr tyT1 tyT2 -> "(" ++ printty ctx tyT1 ++ " -> " ++ printty ctx tyT2 ++ ")"