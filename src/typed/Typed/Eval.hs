module Typed.Eval where

import Typed.Syntax

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmVar i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        TmAbs{} -> True
        TmTrue -> True
        TmFalse -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmApp (TmAbs x ty t12) v2 | isval ctx v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                TmIf TmTrue t2 t3 -> return t2
                TmIf TmFalse t2 t3 -> return t3
                TmIf t1 t2 t3 -> do
                        t1' <- eval' t1
                        return $ TmIf t1' t2 t3
                _ -> Nothing

----------------------------------------------------------------
-- Typing
----------------------------------------------------------------
tyeqv :: MonadFail m => Ty -> Ty -> m ()
tyeqv tyS tyT =
        if tyS == tyT
                then return ()
                else fail $ "type mismatch: " ++ printty tyS ++ ", " ++ printty tyT

typeof :: MonadFail m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                let ctx' = addBinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 tyT2
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv tyT2 tyT11
                                return tyT12
                        _ -> fail "arrow type expected"
        TmTrue -> return TyBool
        TmFalse -> return TyBool
        TmIf t1 t2 t3 -> do
                typeof ctx t1 >>= tyeqv TyBool
                tyT2 <- typeof ctx t2
                typeof ctx t3 >>= tyeqv tyT2
                return tyT2
