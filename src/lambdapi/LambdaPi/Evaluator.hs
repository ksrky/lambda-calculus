module LambdaPi.Evaluator where

import Control.Exception.Safe
import LambdaPi.Syntax

class IsVal a where
        isval :: a -> Bool

instance IsVal Ty where
        isval tyT = case tyT of
                TyStar -> True
                TyTerm t -> isval t

instance IsVal Term where
        isval t = case t of
                TmAbs{} -> True
                TmPi _ t1 t2 -> isval t1 && isval t2
                _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (evalTm t)
    where
        evalTm :: Term -> Maybe Term
        evalTm t = case t of
                TmApp (TmAbs x t11 t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- evalTm t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- evalTm t1
                        Just $ TmApp t1' t2
                TmPi x v1 t2 | isval v1 -> do
                        t2' <- evalTy t2
                        Just $ TmPi x v1 t2'
                TmPi x t1 t2 -> do
                        t1' <- evalTy t1
                        Just $ TmPi x t1' t2
                _ -> Nothing
        evalTy :: Ty -> Maybe Ty
        evalTy tyT = case tyT of
                TyTerm v | isval v -> Nothing
                TyTerm t -> TyTerm <$> evalTm t
                TyStar -> Nothing

----------------------------------------------------------------
-- Type equality check
----------------------------------------------------------------
istmabb :: Context -> Int -> Bool
istmabb ctx i = case getbinding ctx i of
        TmAbbBind tyT _ -> True
        _ -> False

gettmabb :: Context -> Int -> Term
gettmabb ctx i = case getbinding ctx i of
        TmAbbBind tyT _ -> tyT
        _ -> error ""

computetm :: Context -> Term -> Maybe Term
computetm ctx t = case t of
        TmApp (TmAbs _ _ t12) t2 -> Just $ termSubstTop t2 t12
        TmVar i _ | istmabb ctx i -> Just $ gettmabb ctx i
        _ -> Nothing

simplifytm :: Context -> Term -> Term
simplifytm ctx t =
        let t' = case t of
                TmApp tyT1 tyT2 -> TmApp (simplifytm ctx tyT1) tyT2
                _ -> t
         in case computetm ctx t of
                Just t' -> simplifytm ctx t'
                Nothing -> t

tmeqv :: MonadThrow m => Context -> Term -> Term -> m ()
tmeqv ctx t1 t2 = do
        let t1' = simplifytm ctx t1
            t2' = simplifytm ctx t2
        case (t1', t2') of
                (TmVar i _, _) | istmabb ctx i -> tmeqv ctx (gettmabb ctx i) t2'
                (_, TmVar i _) | istmabb ctx i -> tmeqv ctx t1' (gettmabb ctx i)
                (TmVar i _, TmVar j _) | i == j -> return ()
                (TmAbs tyX1 knKS1 tyS2, TmAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                        let ctx' = addname tyX1 ctx
                        tmeqv ctx' tyS2 tyT2
                (TmApp tyS1 tyS2, TmApp tyT1 tyT2) -> do
                        tmeqv ctx tyS1 tyT1
                        tmeqv ctx tyS2 tyT2
                _ -> throwString $ "term mismatch: " ++ printtm ctx t1' ++ ", " ++ printtm ctx t2'

tyeqv :: MonadThrow m => Context -> Ty -> Ty -> m ()
tyeqv ctx tyS tyT = case (tyS, tyT) of
        (TyTerm t1, TyTerm t2) -> tyeqv ctx tyS tyT
        (TyStar, TyStar) -> return ()
        _ -> throwString $ "type mismatch: " ++ printty ctx tyS ++ ", " ++ printty ctx tyT

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
typeof :: MonadThrow m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                let ctx' = addbinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyTerm $ TmPi x tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyTerm (TmPi x tyT11 tyT12) -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> throwString "arrow type expected"
        TmPi x tyT1 tyT2 -> undefined
