module Untyped.Eval where

import Untyped.Syntax (Term (TmAbs, TmApp), termSubstTop)

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        _ -> False

eval :: Term -> Term
eval t = maybe t eval (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmApp (TmAbs x t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                _ -> Nothing
