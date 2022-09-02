module Evaluator where

import Syntax

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmAbs{} -> True
        TmRecord fields -> all (\(l, ti) -> isval ctx ti) fields
        TmVar i _ -> case getbinding ctx i of
                VarBind{} -> True
                _ -> False
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar n _ -> case getbinding ctx n of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmAbs x ty t12) v2 | isval ctx v2 -> Just $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp t1' t2
                TmLet x v1 t2 | isval ctx v1 -> Just $ termSubstTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet x t1' t2
                TmFix v1 | isval ctx v1 -> case v1 of
                        TmAbs _ _ t12 -> Just $ termSubstTop t t12
                        _ -> Nothing
                TmFix t1 -> do
                        t1' <- eval1 t1
                        Just $ TmFix t1'
                TmRecord fields -> do
                        let evalafield :: [(String, Term)] -> Maybe [(String, Term)]
                            evalafield l = case l of
                                [] -> Nothing
                                (l, vi) : rest | isval ctx vi -> do
                                        rest' <- evalafield rest
                                        Just $ (l, vi) : rest'
                                (l, ti) : rest -> do
                                        ti' <- eval1 ti
                                        Just $ (l, ti') : rest
                        fields' <- evalafield fields
                        Just $ TmRecord fields'
                TmTag l v1 tyT2 | isval ctx v1 -> Nothing
                TmTag l t1 tyT2 -> do
                        t1' <- eval1 t1
                        Just $ TmTag l t1' tyT2
                TmCase (TmTag l v11 _) alts | isval ctx v11 -> case lookup l alts of
                        Just (_, body) -> do
                                return $ termSubstTop body v11
                        Nothing -> Nothing
                TmCase t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase t1' alts
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
                                Nothing -> throwString $ "label " ++ li2 ++ " not found"
                (TyRecord _, TyRecord _) -> throwString "field length are not match"
                (TyVariant fields1, TyVariant fields2) | length fields1 == length fields2 -> do
                        zipWithM_
                                ( \(li1, tyTi1) (li2, tyTi2) ->
                                        if li1 == li2
                                                then tyeqv ctx tyTi1 tyTi2
                                                else throwString "label not match"
                                )
                                fields1
                                fields2
                (TyVariant _, TyVariant _) -> throwString "field length are not match"
                (TyUnit, TyUnit) -> return ()
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
        TmLet x t1 t2 -> do
                tyT1 <- typeof ctx t1
                let ctx' = addbinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ typeShift (-1) tyT2
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
        TmProj t1 l -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyRecord fieldtys -> case lookup l fieldtys of
                                Just tyT2 -> return tyT2
                                Nothing -> throwString $ "label " ++ l ++ " not found"
                        _ -> throwString "Expected record type"
        TmCase t1 alts -> do
                tyT <- typeof ctx t1
                case simplifyty ctx tyT of
                        TyVariant fieldtys -> do
                                tys <- forM alts $ \(li, (xi, ti)) -> case lookup li fieldtys of
                                        Just tySi -> do
                                                let ctx' = addbinding xi (VarBind tySi) ctx
                                                tyTi <- typeof ctx ti
                                                return $ typeShift (-1) tyTi
                                        Nothing -> throwString $ "label " ++ li ++ " not found"
                                case tys of
                                        [] -> return $ TyRecord []
                                        tyT1 : restTy -> do
                                                forM_ restTy $ \tyTi -> tyeqv ctx tyTi tyT1
                                                return tyT1
                        _ -> throwString "Expected variant type"
        TmTag l t1 tyT2 -> case simplifyty ctx tyT2 of
                TyVariant fieldtys -> case lookup l fieldtys of
                        Just tyT1Expected -> do
                                tyT1 <- typeof ctx t1
                                tyeqv ctx tyT1 tyT1Expected
                                return tyT2
                        Nothing -> throwString $ "label " ++ l ++ " not found"
                _ -> throwString "Expected variant type"
        TmUnit -> return TyUnit
