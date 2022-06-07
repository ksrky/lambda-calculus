module SystemF.Evaluator where

import SystemF.Syntax (
        Binding (TyVarBind, VarBind),
        Context,
        Term (..),
        Ty (TyAll, TyArr),
        addbinding,
        getTypeFromContext,
        termShift,
        termSubstTop,
        typeShift,
        typeSubstTop,
        tytermSubstTop,
 )

import Control.Monad.State (MonadState (get), State)

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        _ -> False

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

eval1 :: Term -> Maybe Term
eval1 t = case t of
        TmApp (TmAbs x ty t12) v2 | isval v2 -> return $ termSubstTop v2 t12
        TmApp v1 t2 | isval v1 -> do
                t2' <- eval1 t2
                return $ TmApp v1 t2'
        TmApp t1 t2 -> do
                t1' <- eval1 t1
                return $ TmApp t1' t2
        TmTApp (TmTAbs x t11) tyT2 -> return $ tytermSubstTop tyT2 t11
        TmTApp t1 tyT2 -> do
                t1' <- eval1 t1
                return $ TmTApp t1' tyT2
        {-TmUnpack _ _ (TmPack tyT11 v12 _) t2 | isval v12 -> return $ tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
        TmUnpack tyX x t1 t2 -> do
                t1' <- eval1 t1
                return $ TmUnpack tyX x t1' t2
        TmPack tyT1 t2 tyT3 -> do
                t2' <- eval1 t2
                return $ TmPack tyT1 t2' tyT3-}
        _ -> Nothing

typeof :: Term -> State Context Ty
typeof t = case t of
        TmVar i _ -> do
                ctx <- get
                return $ getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
                        TyArr tyT11 tyT12 -> error "parameter type mismatch"
                        _ -> error "arrow type expected"
        TmTAbs tyX t2 -> do
                addbinding tyX TyVarBind
                tyT2 <- typeof t2
                return $ TyAll tyX tyT2
        TmTApp t1 tyT2 -> do
                tyT1 <- typeof t1
                case tyT1 of
                        TyAll _ tyT12 -> return $ typeSubstTop tyT2 tyT12
                        _ -> error "universal type expected"

{-TmPack tyT1 t2 tyT -> case tyT of
        TySome tyY tyT2 -> do
                tyU <- typeof t2
                let tyU' = typeSubstTop tyT1 tyT2
                if tyU == tyU' then return tyT else error "doesn't match declared type"
        _ -> error "existential type expected"
TmUnpack tyX x t1 t2 -> do
        tyT1 <- typeof t1
        case tyT1 of
                TySome tyY tyT11 -> do
                        addbinding tyX TyVarBind
                        addbinding x (VarBind tyT11)
                        tyT2 <- typeof t2
                        return $ typeShift (-2) tyT2
                _ -> error "existential type expected"-}
