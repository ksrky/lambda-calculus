{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Untyped.Syntax where

import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, modify)
import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmAbs String Term
        | TmApp Term Term
        deriving (Show)

data Binding = NameBind

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

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x t2 -> TmAbs x (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

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
                        else "[bad index]" ++ show x ++ show n
        TmAbs x t1 ->
                let (x', ctx') = pickfreshname x ctx
                 in "(λ" ++ x' ++ ". " ++ printtm ctx' t1 ++ ")"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ printtm ctx t2 ++ ")"
