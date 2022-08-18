module CoC.Evaluator where

import CoC.Syntax
import Control.Exception.Safe

isval t = case t of
        TmAbs{} -> True
        TmPi _ t1 t2 -> isval t1 && isval t2
        TmStar -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar i _ -> case getbinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
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

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
tyeqv :: MonadThrow m => Context -> Term -> Term -> m ()
tyeqv ctx tyS tyT = case (tyS, tyT) of
        (TmPi x1 tyT11 tyT12, TmPi x2 tyT21 tyT22) -> do
                tyeqv ctx tyT11 tyT21
                tyeqv ctx tyT21 tyT22
        (TmStar, TmStar) -> return ()
        _ -> throwString $ "type mismatch: " ++ printtm ctx tyS ++ ", " ++ printtm ctx tyT

typeof :: MonadThrow m => Context -> Term -> m Term
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                let ctx' = addbinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TmPi x tyT1 (termShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                case tyT1 of
                        TmPi x tyT11 tyT12 -> do
                                tyT2 <- typeof ctx t2
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> throwString "pi type expected"
        TmPi x tyT1 tyT2 -> undefined
        TmStar -> return TmStar
