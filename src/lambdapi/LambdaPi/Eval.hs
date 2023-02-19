module LambdaPi.Eval where

import LambdaPi.Syntax

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmVar i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        TmAbs{} -> True
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
                _ -> Nothing

evalBinding :: Context -> Binding -> Binding
evalBinding ctx bind = case bind of
        TmAbbBind t tyT -> let t' = eval ctx t in TmAbbBind t' tyT
        _ -> bind

----------------------------------------------------------------
-- Weak head normal forms
----------------------------------------------------------------
whnf :: Term -> Term
whnf (TmApp (TmAbs _ _ t12) t2) = whnf (termSubstTop t2 t12)
whnf (TmApp t1 t2) = TmApp (whnf t1) t2
whnf t = t

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
computetm ctx (TmVar i _) | istmabb ctx i = computetm ctx (gettmabb ctx i)
computetm _ _ = Nothing

whred :: Bool -> Context -> Term -> Term
whred True ctx t = case computetm ctx t of
        Just t' -> whred True ctx t'
        Nothing -> t
whred False _ t = t

tmeqv :: MonadFail m => Context -> Term -> Term -> m ()
tmeqv ctx s t = do
        let s' = whred True ctx s
            t' = whred True ctx t
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
                                _ -> fail "Pi type required"
                (TmAbs x tyS1 tmS2, TmAbs _ _ tmT2) -> do
                        -- tmp: equality check between tyS1 and tyS2
                        let ctx' = addBinding x (VarBind tyS1) ctx
                         in tmeqv ctx' (whnf tmS2) (whnf tmT2)
                (_, TmAbs x tyT2 t2) -> do
                        let ctx' = addBinding x (VarBind tyT2) ctx
                        tmeqv ctx' (whnf (TmApp s' (TmVar 0 (length ctx')))) (whnf t2)
                (TmAbs x tyS2 s2, _) -> do
                        let ctx' = addBinding x (VarBind tyS2) ctx
                        tmeqv ctx' (whnf s2) (whnf (TmApp t' (TmVar 0 (length ctx'))))
                _ -> fail $ "term mismatch: " ++ printtm ctx s ++ ", " ++ printtm ctx t

----------------------------------------------------------------
-- Type equivalence
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: Context -> Int -> Type
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> tyT
        _ -> error "unreachable"

computety :: Context -> Type -> Maybe Type
computety ctx tyT = case tyT of
        TyApp (TyPi _ _ tyT12) t2 -> Just $ termtySubstTop t2 tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Type -> Type
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT of
                Just tyT'' -> simplifyty ctx tyT''
                Nothing -> tyT'

tyeqv :: MonadFail m => Context -> Type -> Type -> m ()
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
                                KnPi _ tyT11 _ -> tyeqv ctx tyT11 tyT2
                                _ -> fail "Pi kind required"
                        tmeqv ctx t1 t2
                (TyPi x tyS1 tyS2, TyPi _ tyT1 tyT2) -> do
                        tyeqv ctx tyS1 tyT1
                        checkKnStar ctx tyT1
                        let ctx' = addBinding x (VarBind tyS1) ctx
                        tyeqv ctx' tyS2 tyT2
                        checkKnStar ctx' tyT2
                _ -> fail $ "type mismatch: " ++ printty ctx tyS ++ ", " ++ printty ctx tyT

----------------------------------------------------------------
-- Kind equivalence
----------------------------------------------------------------
kneqv :: MonadFail m => Context -> Kind -> Kind -> m ()
kneqv _ KnStar KnStar = return ()
kneqv ctx (KnPi x tyT1 knK1) (KnPi _ tyT2 knK2) = do
        tyeqv ctx tyT1 tyT2
        checkKnStar ctx tyT2
        let ctx' = addBinding x (VarBind tyT1) ctx
        kneqv ctx' knK1 knK2
kneqv ctx knK1 knK2 = fail $ "kind mismatch: " ++ printkn ctx knK1 ++ ", " ++ printkn ctx knK2

----------------------------------------------------------------
-- Typing
----------------------------------------------------------------
checkType :: MonadFail m => Context -> Term -> Type -> m ()
checkType ctx t tyT = do
        tyT' <- typeof ctx t
        tyeqv ctx tyT tyT'

typeof :: MonadFail m => Context -> Term -> m Type
typeof ctx (TmVar i _) = getType ctx i
typeof ctx (TmApp t1 t2) = do
        tyT1 <- typeof ctx t1
        tyT2 <- typeof ctx t2
        case tyT1 of
                TyPi _ tyS11 tyT12 -> do
                        tyeqv ctx tyS11 tyT2
                        return $ termtySubstTop t2 tyT12
                _ -> fail "Pi type required"
typeof ctx (TmAbs x tyS1 t2) = do
        checkKnStar ctx tyS1
        let ctx' = addBinding x (VarBind tyS1) ctx
        tyT2 <- typeof ctx' t2
        return $ TyPi x tyS1 tyT2

----------------------------------------------------------------
-- Kinding
----------------------------------------------------------------
checkKnStar :: MonadFail m => Context -> Type -> m ()
checkKnStar ctx tyT = do
        knK <- kindof ctx tyT
        kneqv ctx knK KnStar

kindof :: MonadFail m => Context -> Type -> m Kind
kindof ctx (TyVar i _) = getKind ctx i
kindof ctx (TyApp tyS1 t2) = do
        knK1 <- kindof ctx tyS1
        tyT2 <- typeof ctx t2
        case knK1 of
                KnPi _ tyT11 knK12 -> do
                        tyeqv ctx tyT11 tyT2
                        return knK12
                _ -> fail ""
kindof ctx (TyPi x tyT1 tyT2) = do
        checkKnStar ctx tyT1
        let ctx' = addBinding x (VarBind tyT1) ctx
        checkKnStar ctx' tyT2
        return KnStar

checkBinding :: MonadFail m => Context -> Binding -> m Binding
checkBinding ctx bind = case bind of
        NameBind -> return NameBind
        TyVarBind knK -> return $ TyVarBind knK
        TyAbbBind tyT Nothing -> TyAbbBind tyT . Just <$> kindof ctx tyT
        TyAbbBind tyT (Just knK) -> do
                knK' <- kindof ctx tyT
                kneqv ctx knK knK'
                return $ TyAbbBind tyT (Just knK)
        VarBind tyT -> return $ VarBind tyT
        TmAbbBind t Nothing -> TmAbbBind t . Just <$> typeof ctx t
        TmAbbBind t (Just tyT) -> do
                tyT' <- typeof ctx t
                tyeqv ctx tyT tyT'
                return $ TmAbbBind t (Just tyT)