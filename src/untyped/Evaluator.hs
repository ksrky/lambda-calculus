module Evaluator where

import Control.Monad.State

import Syntax

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
        walk c t = case t of
                TmVar x n ->
                        if x >= c
                                then TmVar (x + d) (n + d)
                                else TmVar x (n + d)
                TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
    where
        walk c t = case t of
                TmVar x n -> if x == j + c then termShift c s else TmVar x n
                TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Term -> Bool
isval t = case t of
        TmAbs _ _ -> True
        _ -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
        TmApp (TmAbs x t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
        TmApp v1 t2 | isval v1 -> do
                t2' <- eval1 ctx t2
                Just $ TmApp v1 t2'
        TmApp t1 t2 -> do
                t1' <- eval1 ctx t1
                Just $ TmApp t1' t2
        _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
        Just t' -> eval ctx t'
        Nothing -> t
