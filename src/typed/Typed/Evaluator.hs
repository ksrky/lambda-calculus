module Typed.Evaluator where

import Typed.Syntax

import Control.Exception.Safe
import Control.Monad.State

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

typeof :: MonadThrow m => Term -> CT m Ty
typeof t = case t of
        TmVar i _ -> do
                ctx <- get
                getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ TyArr tyT1 tyT2
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv tyT2 tyT11
                                return tyT12
                        _ -> throwString "arrow type expected"
        TmTrue -> return TyBool
        TmFalse -> return TyBool
        TmIf t1 t2 t3 -> do
                typeof t1 >>= tyeqv TyBool
                tyT2 <- typeof t2
                typeof t3 >>= tyeqv tyT2
                return tyT2

tyeqv :: MonadThrow m => Ty -> Ty -> m ()
tyeqv tyS tyT =
        if tyS == tyT
                then return ()
                else throwString $ "type mismatch: " ++ show tyS ++ ", " ++ show tyT
