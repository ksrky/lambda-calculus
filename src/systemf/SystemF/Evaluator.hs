module SystemF.Evaluator where

import SystemF.Syntax

import Control.Exception.Safe
import Control.Monad.State (MonadState (get), State)

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmTAbs{} -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar i _ -> case getbinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmAbs _ _ t12) v2 | isval v2 -> return $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        return $ TmApp t1' t2
                TmTApp (TmTAbs _ t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp t1' tyT2
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
        TmTAbs tyX t2 -> do
                addbinding tyX TyVarBind
                tyT2 <- typeof t2
                return $ TyAll tyX tyT2
        TmTApp t1 tyT2 -> do
                tyT1 <- typeof t1
                case tyT1 of
                        TyAll _ tyT12 -> return $ typeSubstTop tyT2 tyT12
                        _ -> throwString "universal type expected"

tyeqv :: MonadThrow m => Ty -> Ty -> m ()
tyeqv tyS tyT =
        if tyS == tyT
                then return ()
                else throwString $ "type mismatch: " ++ show tyS ++ ", " ++ show tyT
