module FOmega.Eval where

import FOmega.Syntax

import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmVar i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        TmAbs{} -> True
        TmTAbs{} -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmVar i _ -> case getBinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmAbs x ty t12) v2 | isval ctx v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                TmTApp (TmTAbs x _ t11) tyT2 -> Just $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval' t1
                        Just $ TmTApp t1' tyT2
                _ -> Nothing

----------------------------------------------------------------
-- Type equivalence
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> tyT
        _ -> error "unreachable"

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyApp (TyAbs _ _ tyT12) tyT2 -> Just $ typeSubstTop tyT2 tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT of
                Just tyT' -> simplifyty ctx tyT'
                Nothing -> tyT

tyeqv :: MonadFail m => Context -> Ty -> Ty -> m ()
tyeqv ctx tyS tyT = do
        let tyS' = simplifyty ctx tyS
            tyT' = simplifyty ctx tyT
        case (tyS', tyT') of
                (TyVar i _, TyVar j _) | i == j -> return ()
                (TyVar i _, _) | istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
                (_, TyVar i _) | istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
                (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
                        tyeqv ctx tyS1 tyT1
                        tyeqv ctx tyS2 tyT2
                (TyAll tyX1 _ tyS2, TyAll _ _ tyT2) -> do
                        let ctx' = addName tyX1 ctx
                        tyeqv ctx' tyS2 tyT2
                (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> do
                        tyeqv ctx tyS1 tyT1
                        tyeqv ctx tyS2 tyT2
                (TyAbs tyX1 knKS1 tyS2, TyAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                        let ctx' = addName tyX1 ctx
                        tyeqv ctx' tyS2 tyT2
                _ -> fail $ "type mismatch: " ++ printty ctx tyS ++ ", " ++ printty ctx tyT

----------------------------------------------------------------
-- Typing
----------------------------------------------------------------
typeof :: MonadFail m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> fail "arrow type expected"
        TmAbs x tyT1 t2 -> do
                checkKnStar ctx tyT1
                let ctx' = addBinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmTApp t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ knK11 tyT12 | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                        TyAll _ knK11 tyT12 -> fail "Type argument has wrong kind"
                        _ -> fail "universal type expected"
        TmTAbs tyX knK1 t2 -> do
                let ctx' = addBinding tyX (TyVarBind knK1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll tyX knK1 tyT2

----------------------------------------------------------------
-- Kinding
----------------------------------------------------------------
checkKnStar :: MonadFail m => Context -> Ty -> m ()
checkKnStar ctx tyT = do
        knK <- kindof ctx tyT
        case knK of
                KnStar -> return ()
                _ -> fail "Kind * expected"

kindof :: MonadFail m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyVar i _ -> getKindFromContext ctx i
        TyArr tyT1 tyT2 -> do
                checkKnStar ctx tyT1
                checkKnStar ctx tyT2
                return KnStar
        TyAbs tyX knK1 tyT2 -> do
                let ctx' = addBinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr knK11 knK12 -> fail "parameter kind mismatch"
                        _ -> fail "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                let ctx' = addBinding tyX (TyVarBind knK1) ctx
                checkKnStar ctx' tyT2
                return KnStar
