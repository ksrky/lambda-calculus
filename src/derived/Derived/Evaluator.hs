module Derived.Evaluator where

import Derived.Syntax

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isnumericval :: Term -> Bool
isnumericval t = case t of
        TmZero -> True
        TmSucc t1 -> isnumericval t1
        _ -> False

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmApp (TmFold _) v -> isval v
        TmRecord fields -> all (\(l, ti) -> isval ti) fields
        t | isnumericval t -> True
        TmTrue -> True
        TmFalse -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar n _ -> case getbinding ctx n of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmUnfold tyS) (TmApp (TmFold tyT) v) | isval v -> Just v
                TmApp (TmFold tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp (TmFold tyS) t2'
                TmApp (TmUnfold tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp (TmUnfold tyS) t2'
                TmApp (TmAbs x ty t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp t1' t2
                TmFix v1 | isval v1 -> case v1 of
                        TmAbs _ _ t12 -> Just $ termSubstTop t t12
                        _ -> Nothing
                TmFix t1 -> do
                        t1' <- eval1 t1
                        Just $ TmFix t1'
                TmRecord fields -> do
                        let evalafield :: [(String, Term)] -> Maybe [(String, Term)]
                            evalafield l = case l of
                                [] -> Nothing
                                (l, vi) : rest | isval vi -> do
                                        rest' <- evalafield rest
                                        Just $ (l, vi) : rest'
                                (l, ti) : rest -> do
                                        ti' <- eval1 ti
                                        Just $ (l, ti') : rest
                        fields' <- evalafield fields
                        Just $ TmRecord fields'
                TmTag l vs tyT | all isval vs -> Nothing
                TmTag l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag l ts' tyT
                TmCase (TmTag li vs11 _) alts | all isval vs11 -> case lookup li alts of
                        Just (pat, body) -> do
                                return $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase t1' alts
                TmSucc t1 -> do
                        t1' <- eval1 t1
                        Just $ TmSucc t1'
                TmIf TmTrue t2 t3 -> Just t2
                TmIf TmFalse t2 t3 -> Just t3
                TmIf t1 t2 t3 -> do
                        t1' <- eval1 t1
                        Just $ TmIf t1' t2 t3
                _ -> Nothing

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getbinding ctx i of
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

tyeqv :: MonadThrow m => Context -> Ty -> Ty -> m ()
tyeqv ctx tyS tyT = do
        let tyS' = simplifyty ctx tyS
            tyT' = simplifyty ctx tyT
        case (tyS', tyT') of
                (TyVar i _, _) | istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
                (_, TyVar i _) | istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
                (TyVar i _, TyVar j _) | i == j -> return ()
                (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
                        tyeqv ctx tyS1 tyT1
                        tyeqv ctx tyS2 tyT2
                (TyRecord fields1, TyRecord fields2) | length fields1 == length fields2 -> forM_ fields2 $
                        \(li2, tyTi2) -> case lookup li2 fields1 of
                                Just tyTi1 -> tyeqv ctx tyTi1 tyTi2
                                Nothing -> throwString ""
                (TyRecord _, TyRecord _) -> throwString "field length are not match"
                (TyBool, TyBool) -> return ()
                (TyNat, TyNat) -> return ()
                _ -> throwString "type mismatch"

typeof :: MonadThrow m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                let ctx' = addbinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 tyT2
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> throwString "arrow type expected"
        TmFix t1 -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT12 tyT11
                                return tyT12
                        _ -> throwString "arrow type expected"
        TmRecord fields -> do
                fieldtys <- forM fields $ \(li, ti) -> do
                        tyTi <- typeof ctx ti
                        return (li, tyTi)
                return $ TyRecord fieldtys
        TmTag li ts1 tyT2 -> case simplifyty ctx tyT2 of
                TyVariant fieldtys -> case lookup li fieldtys of
                        Just tyTsExpected -> do
                                tyTs <- mapM (typeof ctx) ts1
                                mapM_ (uncurry $ tyeqv ctx) (zip tyTs tyTsExpected)
                                return tyT2
                        Nothing -> throwString $ "label " ++ li ++ " not found"
                _ -> throwString "Expected variant type"
        TmCase t alts -> undefined {-do
                                   tyT <- typeof ctx t
                                   case simplifyty ctx tyT of
                                           TyVariant fieldtys -> do
                                                   when (null fieldtys) $ return ()
                                                   (tyT1 : restTy) <- forM alts $ \(li, (ki, ti)) -> case lookup li fieldtys of
                                                           Just tys -> do
                                                                   ctx' <- (`execStateT` ctx) $
                                                                           forM_ tys $ \tyT -> StateT $ \ctx -> do
                                                                                   let ctx' = addbinding dummyName (VarBind tyT) ctx
                                                                                   return ((), ctx')
                                                                   tyTi <- typeof ctx' ti
                                                                   return $ typeShift (- ki) tyTi
                                                           Nothing | nullName li -> do
                                                                   let ctx' = addbinding dummyName (VarBind $ TyVariant fieldtys) ctx
                                                                   tyTi <- typeof ctx' ti
                                                                   return $ typeShift (- ki) tyTi
                                                           Nothing -> throwString $ "label " ++ name2str li ++ " not found"
                                                   forM_ restTy $ \tyTi -> tyeqv fi ctx tyTi tyT1
                                                   return tyT1
                                           _ -> throwString "Expected variant type"-}
        TmFold tyS -> case simplifyty ctx tyS of
                TyRec _ tyT -> return $ TyArr (typeSubstTop tyS tyT) tyS
                _ -> throwString "recursive type expected"
        TmUnfold tyS -> case simplifyty ctx tyS of
                TyRec _ tyT -> return $ TyArr tyS (typeSubstTop tyS tyT)
                _ -> throwString "recursive type expected"
        TmSucc t1 -> do
                tyT1 <- typeof ctx t1
                tyeqv ctx tyT1 TyNat
                return TyNat
        TmZero -> return TyNat
        TmTrue -> return TyBool
        TmFalse -> return TyBool
        TmIf t1 t2 t3 -> do
                typeof ctx t1 >>= tyeqv ctx TyBool
                tyT2 <- typeof ctx t2
                typeof ctx t3 >>= tyeqv ctx tyT2
                return tyT2
