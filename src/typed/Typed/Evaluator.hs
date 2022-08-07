module Typed.Evaluator where

import Typed.Syntax (
        Term (TmAbs, TmApp, TmFalse, TmIf, TmTrue),
        termSubstTop,
 )

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmTrue -> True
        TmFalse -> True
        _ -> False

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmApp (TmAbs x ty t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp t1' t2
                TmIf TmTrue t2 t3 -> return t2
                TmIf TmFalse t2 t3 -> return t3
                TmIf t1 t2 t3 -> do
                        t1' <- eval1 t1
                        return $ TmIf t1' t2 t3
                _ -> Nothing
