module LambdaPi.Evaluator where

import LambdaPi.Syntax

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmStar -> True
        TmPi _ t1 t2 -> isval t1 && isval t2
        _ -> False

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmApp (TmAbs x t11 t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp t1' t2
                TmPi x v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        Just $ TmPi x v1 t2'
                TmPi x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmPi x t1' t2
                _ -> Nothing
