module Evaluator where

import Control.Monad.State
import Syntax

isnumericval :: Term -> Bool
isnumericval t = case t of
        TmZero -> True
        TmSucc t1 -> isnumericval t1
        _ -> False

isval :: Term -> Bool
isval t = case t of
        TmString _ -> True
        TmTrue -> True
        TmFalse -> True
        TmTag l t1 _ -> isval t1
        TmUnit -> True
        TmFloat _ -> True
        TmAbs{} -> True
        TmRecord fields -> all (\(l, ti) -> isval ti) fields
        t -> isnumericval t

type Eval a = StateT Context Maybe a
type Error a = Either String a

eval1 :: Term -> Eval Term
eval1 t = case t of
        TmAscribe v1 tyT | isval v1 -> return v1
        TmAscribe t1 tyT -> do
                t1' <- eval1 t1
                return $ TmAscribe t1' tyT
        TmIf TmTrue t2 t3 -> return t2
        TmIf TmFalse t2 t3 -> return t3
        TmIf t1 t2 t3 -> do
                t1' <- eval1 t1
                return $ TmIf t1' t2 t3
        TmTag l t1 tyT -> do
                t1' <- eval1 t1
                return $ TmTag l t1' tyT
        TmCase (TmTag li v11 _) branches | isval v11 -> case lookup li branches of
                Just (_, body) -> return $ termSubstTop v11 body
                Nothing -> lift Nothing
        TmCase t1 branches -> do
                t1' <- eval1 t1
                return $ TmCase t1' branches
        TmVar n _ -> do
                ctx <- get
                case getbinding ctx n of
                        TmAbbBind t _ -> return t
                        _ -> lift Nothing
        TmTimesfloat (TmFloat f1) (TmFloat f2) -> return $ TmFloat (f1 * f2)
        TmTimesfloat t1@TmFloat{} t2 -> do
                t2' <- eval1 t2
                return $ TmTimesfloat t1 t2'
        TmTimesfloat t1 t2 -> do
                t1' <- eval1 t1
                return $ TmTimesfloat t1' t2
        TmLet x v1 t2 | isval v1 -> return $ termSubstTop v1 t2
        TmLet x t1 t2 -> do
                t1' <- eval1 t1
                return $ TmLet x t1' t2
        TmRecord fields -> do
                let evalafield :: [(String, Term)] -> Eval [(String, Term)]
                    evalafield l = case l of
                        [] -> lift Nothing
                        (l, vi) : rest | isval vi -> do
                                rest' <- evalafield rest
                                return $ (l, vi) : rest'
                        (l, ti) : rest -> do
                                ti' <- eval1 ti
                                return $ (l, ti') : rest
                fields' <- evalafield fields
                return $ TmRecord fields'
        TmProj (TmRecord fields) l -> lift $ lookup l fields
        TmProj t1 l -> do
                t1' <- eval1 t1
                return $ TmProj t1' l
        TmApp (TmAbs x tyT11 t12) v2 | isval v2 -> return $ termSubstTop v2 t12
        TmApp v1 t2 | isval v1 -> do
                t2' <- eval1 t2
                return $ TmApp v1 t2'
        TmApp t1 t2 -> do
                t1' <- eval1 t1
                return $ TmApp t1' t2
        TmFix v1 | isval v1 -> case v1 of
                TmAbs _ _ t12 -> return $ termSubstTop t t12
                _ -> lift Nothing
        TmFix t1 -> do
                t1' <- eval1 t1
                return $ TmFix t1'
        TmSucc t1 -> do
                t1' <- eval1 t1
                return $ TmSucc t1'
        TmPred TmZero -> return TmZero
        TmPred (TmSucc nv1) | isnumericval nv1 -> return nv1
        TmPred t1 -> do
                t1' <- eval1 t1
                return $ TmPred t1'
        TmIsZero TmZero -> return TmTrue
        TmIsZero (TmSucc nv1) | isnumericval nv1 -> return TmFalse
        TmIsZero t1 -> do
                t1' <- eval1 t1
                return $ TmIsZero t1'
        _ -> lift Nothing

eval :: Term -> Eval Term
eval t = do
        t' <- eval1 t
        eval t'

evalbinding :: Binding -> Eval Binding
evalbinding b = case b of
        TmAbbBind t tyT -> do
                t' <- eval t
                return $ TmAbbBind t' tyT
        bind -> return bind

istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT -> True
        _ -> False

gettyabb :: Context -> Int -> Maybe Ty
gettyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT -> Just tyT
        _ -> Nothing

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyVar i _ | istyabb ctx i -> gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT = case computety ctx tyT of
        Just tyT' -> simplifyty ctx tyT'
        Nothing -> tyT

tyeqv :: Context -> Ty -> Ty -> Bool
tyeqv ctx tyS tyT = do
        let tyS = simplifyty ctx tyS
        let tyT = simplifyty ctx tyT
        case (tyS, tyT) of
                (TyUnit, TyUnit) -> True
                (TyId b1, TyId b2) -> b1 == b2
                (TyVariant fields1, TyVariant fields2) -> all (\((li1, tyTi1), (li2, tyTi2)) -> li1 == li2 && tyeqv ctx tyTi1 tyTi2) (zip fields1 fields2)
                (TyString, TyString) -> True
                (TyFloat, TyFloat) -> True
                (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
                (TyBool, TyBool) -> True
                (TyNat, TyNat) -> True
                (TyRecord fields1, TyRecord fields2) ->
                        all
                                ( \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> tyeqv ctx tyTi1 tyTi2
                                        Nothing -> False
                                )
                                fields2
                (TyVar i _, _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx ty tyT
                        Nothing -> False
                (_, TyVar i _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx tyS ty
                        Nothing -> False
                (TyVar i _, TyVar j _) -> i == j
                _ -> False

typeof :: Context -> Term -> Ty
typeof ctx t = case t of
        TmAscribe t1 tyT ->
                if tyeqv ctx (typeof ctx t1) tyT
                        then tyT
                        else error "body of as-term does not have the expected type"
        TmString _ -> TyString
        TmTrue -> TyBool
        TmFalse -> TyBool
        TmIf t1 t2 t3 ->
                if tyeqv ctx (typeof ctx t1) TyBool
                        then
                                let tyT2 = typeof ctx t2
                                 in if tyeqv ctx tyT2 (typeof ctx t3)
                                        then tyT2
                                        else error $ "arms of conditional have different types"
                        else error "guard of conditional not a boolean"
        TmCase t cases -> do
                case simplifyty ctx (typeof ctx t) of
                        TyVariant fieldtys -> undefined
                                where
                                        checkField (li, (xi, ti)) = case lookup li fieldtys of
                                                Just _ -> do
                                                       let tyTi = case lookup li fieldtys of
                                                                Just tyTi -> tyTi
                                                                Nothing -> error "label " ++ show li ++ " not found"
                                                        addbinding xi (VarBind tyTi) typeShift (-1) (typeof ctx' ti)
                                                        let tyT1 : restTy = map (\(li, (xi, ti)) -> typeShift (-1) (typeof ti)) cases
                                                        mapM (\tyTi -> if not (tyeqv ctx tyTi tyT1) then error "fields do not have the same type" else return ()) restTy
                                                        return ()
                                                Nothing -> error $ "label " ++ show li ++ " not in type"
                                map
                                        ( \(li, (xi, ti)) -> case lookup li fieldtys of
                                                Just _ -> do
                                                        let tyTi = case lookup li fieldtys of
                                                                Just tyTi -> tyTi
                                                                Nothing -> error "label " ++ show li ++ " not found"
                                                        addbinding xi (VarBind tyTi) typeShift (-1) (typeof ctx' ti)
                                                        let tyT1 : restTy = map (\(li, (xi, ti)) -> typeShift (-1) (typeof ti)) cases
                                                        mapM (\tyTi -> if not (tyeqv ctx tyTi tyT1) then error "fields do not have the same type" else return ()) restTy
                                                        return tyT1
                                                Nothing -> error $ "label " ++ show li ++ " not in type"
                                        )
                                        cases
                        _ -> error "Expected variant type"
        TmTag li ti tyT -> case simplifyty ctx tyT of
                TyVariant fieldtys -> do
                        case lookup li fieldtys of
                                Just tyTiExpected -> do
                                        tyTi <- typeof ti
                                        if tyeqv ctx tyTi tyTiExpected
                                                then tyT
                                                else error "field does not have expected type"
                                Nothing -> error "label " ++ show li ++ " not found"
                _ -> error "Annotation is not a variant type"
        TmUnit -> TyUnit
        TmFloat _ -> TyFloat
        TmTimesfloat t1 t2 ->
                if tyeqv ctx (typeof ctx t1) TyFloat && tyeqv ctx (typeof ctx t2) TyFloat
                        then TyFloat
                        else error "argument of timesfloat is not a number"
        TmVar i _ -> getTypeFromContext ctx i
        TmLet x t1 t2 -> do
                tyT1 <- typeof t1
                addbinding x (VarBind tyT1)
                typeShift (-1) (typeof ctx' t2)
        TmRecord fields -> do
                let fieldtys = map (\(li, ti) -> (li, typeof ctx ti)) fields
                TyRecord fieldtys
        TmProj t1 l -> case simplifyty ctx (typeof ctx t1) of
                TyRecord (fieldtys) -> case lookup l fieldtys of
                        Just _ -> undefined
                        Nothing -> error $ "label " ++ show l ++ " not found"
                _ -> error "Expected record type"
        TmInert tyT -> tyT
        TmAbs x tyT1 t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case simplifyty ctx tyT1 of
                        TyArr tyT11 tyT12 ->
                                if tyeqv ctx tyT2 tyT11
                                        then tyT12
                                        else error "parameter type mismatch"
                        _ -> error "arrow type expected"
        TmFix t1 -> do
                tyT1 <- typeof t1
                case simplifyty ctx tyT1 of
                        TyArr tyT11 tyT12 ->
                                if tyeqv ctx tyT12 tyT11
                                        then tyT12
                                        else error "result of body not compatible with domain"
                        _ -> error "arrow type expected"
        TmZero -> TyNat
        TmSucc t1 ->
                if tyeqv ctx (typeof ctx t1) TyNat
                        then TyNat
                        else error "argument of succ is not a number"
        TmPred t1 -> do
                typeof t1
                if tyeqv ctx (typeof ctx t1) TyNat
                        then TyNat
                        else error "argument of pred is not a number"
        TmIsZero (fi, t1) -> do
                typeof t1
                if tyeqv ctx t1' TyNat then TyBool else error "argument of iszero is not a number"