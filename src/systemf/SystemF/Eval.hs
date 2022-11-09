module SystemF.Eval where

import SystemF.Syntax

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
                TmApp (TmAbs _ _ t12) v2 | isval ctx v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                TmTApp (TmTAbs _ t11) tyT2 -> Just $ tytermSubstTop tyT2 t11
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
        TyAbbBind tyT -> tyT
        _ -> error ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT = case computety ctx tyT of
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
                (TyAll tyX1 tyS2, TyAll _ tyT2) -> do
                        let ctx' = addBinding tyX1 TyVarBind ctx
                        tyeqv ctx' tyS2 tyT2
                _ -> fail $ "type mismatch: " ++ printty ctx tyS ++ ", " ++ printty ctx tyT

typeof :: MonadFail m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                let ctx' = addBinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> fail "arrow type expected"
        TmTAbs tyX t2 -> do
                let ctx' = addBinding tyX TyVarBind ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll tyX tyT2
        TmTApp t1 tyT2 -> do
                tyT1 <- typeof ctx t1
                case tyT1 of
                        TyAll _ tyT12 -> return $ typeSubstTop tyT2 tyT12
                        _ -> fail "universal type expected"
