module LambdaPi.Evaluator where

import Control.Exception.Safe
import LambdaPi.Syntax

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: a -> Bool
isval = undefined

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmVar i _ -> case getBinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmAbs x t11 t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                _ -> Nothing

----------------------------------------------------------------
-- Term equivalence
----------------------------------------------------------------
istmabb :: Context -> Int -> Bool
istmabb ctx i = case getBinding ctx i of
        TmAbbBind{} -> True
        _ -> False

gettmabb :: Context -> Int -> Term
gettmabb ctx i = case getBinding ctx i of
        TmAbbBind t _ -> t
        _ -> error "unreachable"

computetm :: Context -> Term -> Maybe Term
computetm ctx tyT = case tyT of
        TmApp (TmAbs _ _ t12) t2 -> Just $ termSubstTop t2 t12
        TmVar i _ | istmabb ctx i -> Just $ gettmabb ctx i
        _ -> Nothing

simplifytm :: Context -> Term -> Term
simplifytm ctx t =
        let t' = case t of
                TmApp t1 t2 -> TmApp (simplifytm ctx t1) t2
                _ -> t
         in case computetm ctx t' of
                Just t'' -> simplifytm ctx t''
                Nothing -> t'

tmeqv :: MonadThrow m => Context -> Term -> Term -> m ()
tmeqv ctx s t = do
        let s' = simplifytm ctx s
            t' = simplifytm ctx t
        case (s', t') of
                (TmVar i _, TmVar j _) | i == j -> return ()
                (TmVar i _, _) | istmabb ctx i -> tmeqv ctx (gettmabb ctx i) t
                (_, TmVar i _) | istmabb ctx i -> tmeqv ctx s (gettmabb ctx i)
                (TmApp s1 s2, TmApp t1 t2) -> do
                        tmeqv ctx s1 t1
                        tyS1 <- typeof ctx s1
                        tmeqv ctx s2 t2
                        tyS2 <- typeof ctx s2
                        case tyS1 of
                                TyPi _ tyS11 _ -> tyeqv ctx tyS11 tyS2
                                _ -> throwString ""
                (TmApp (TmAbs x tyS1 t1) s2, t2) -> do
                        let ctx' = addBinding x (VarBind tyS1) ctx
                        checkType ctx' s tyS1
                _ -> throwString ""

----------------------------------------------------------------
-- Type equivalence
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> tyT
        _ -> error ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyApp (TyPi _ _ tyT12) t2 -> Just $ termtySubstTop t2 tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT of
                Just tyT'' -> simplifyty ctx tyT''
                Nothing -> tyT'

tyeqv :: MonadThrow m => Context -> Ty -> Ty -> m ()
tyeqv ctx tyS tyT = do
        let tyS' = simplifyty ctx tyS
            tyT' = simplifyty ctx tyT
        case (tyS', tyT') of
                (TyVar i _, TyVar j _) | i == j -> return ()
                (TyVar i _, _) | istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
                (_, TyVar i _) | istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
                (TyApp tyS1 t1, TyApp tyS2 t2) -> do
                        tyeqv ctx tyS1 tyS2
                        knK1 <- kindof ctx tyS1
                        tyT2 <- typeof ctx t2
                        case knK1 of
                                KnPi _ tyT11 knK12 -> tyeqv ctx tyT11 tyT2
                                _ -> throwString ""
                        tmeqv ctx t1 t2
                (TyPi x tyS1 tyS2, TyPi y tyT1 tyT2) | x == y -> do
                        tyeqv ctx tyS1 tyT1
                        knK1 <- kindof ctx tyT1
                        kneqv ctx knK1 KnStar
                        let ctx' = addBinding x (VarBind tyS1) ctx
                        tyeqv ctx' tyS2 tyT2
                        knK2 <- kindof ctx' tyT2
                        kneqv ctx' knK2 KnStar
                _ -> throwString $ "type mismatch: " ++ printty ctx tyS ++ ", " ++ printty ctx tyT

----------------------------------------------------------------
-- Kind equivalence
----------------------------------------------------------------
kneqv :: MonadThrow m => Context -> Kind -> Kind -> m ()
kneqv ctx KnStar KnStar = return ()
kneqv ctx (KnPi x tyT1 knK1) (KnPi _ tyT2 knK2) = do
        tyeqv ctx tyT1 tyT2
        checkKnStar ctx tyT2
        let ctx' = addBinding x (VarBind tyT1) ctx
        kneqv ctx' knK1 knK2
kneqv ctx knK1 knK2 = throwString $ "kind mismatch: " ++ printkn ctx knK1 ++ ", " ++ printkn ctx knK2

----------------------------------------------------------------
-- Typing
----------------------------------------------------------------
checkType :: MonadThrow m => Context -> Term -> Ty -> m ()
checkType ctx t tyT = do
        tyT' <- typeof ctx t
        tyeqv ctx tyT tyT'

typeof :: MonadThrow m => Context -> Term -> m Ty
typeof ctx (TmVar i _) = getTypeFromContext ctx i
typeof ctx (TmApp t1 t2) = do
        tyT1 <- typeof ctx t1
        tyT2 <- typeof ctx t2
        case tyT1 of
                TyPi x tyS11 tyT12 -> do
                        tyeqv ctx tyS11 tyT2
                        return tyT12
                _ -> throwString ""
typeof ctx (TmAbs x tyS1 t2) = do
        checkKnStar ctx tyS1
        let ctx' = addBinding x (VarBind tyS1) ctx
        tyT2 <- typeof ctx' t2
        return $ TyPi x tyS1 (typeShift (-1) tyT2)

----------------------------------------------------------------
-- Kinding
----------------------------------------------------------------
checkKnStar :: MonadThrow m => Context -> Ty -> m ()
checkKnStar ctx tyT = do
        knK <- kindof ctx tyT
        kneqv ctx knK KnStar

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx (TyVar i _) = getKindFromContext ctx i
kindof ctx (TyApp tyS1 t2) = do
        knK1 <- kindof ctx tyS1
        tyT2 <- typeof ctx t2
        case knK1 of
                KnPi _ tyT11 knK12 -> do
                        tyeqv ctx tyT11 tyT2
                        return knK12
                _ -> throwString ""
kindof ctx (TyPi x tyT1 tyT2) = do
        checkKnStar ctx tyT1
        let ctx' = addBinding x (VarBind tyT1) ctx
        checkKnStar ctx' tyT2
        return KnStar
