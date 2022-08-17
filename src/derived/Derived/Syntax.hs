module Derived.Syntax where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyRecord [(String, Ty)]
        | TyVariant [(String, [Ty])]
        | TyRec String Ty
        | TyBool
        | TyNat
        deriving (Eq, Show)

data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmFix Term
        | TmRecord [(String, Term)]
        | TmCase Term [(String, (Int, Term))]
        | TmTag String [Term] Ty
        | TmFold Ty
        | TmUnfold Ty
        | -- Base
          TmSucc Term
        | TmZero
        | TmTrue
        | TmFalse
        | TmIf Term Term Term
        deriving (Show)

data Binding
        = NameBind
        | VarBind Ty
        | TmAbbBind Term (Maybe Ty)
        | TyVarBind
        | TyAbbBind Ty
        deriving (Show)

data Command
        = Bind String Binding
        | Eval Term
        deriving (Show)

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
type Context = [(String, Binding)]

emptyContext :: Context
emptyContext = []

addbinding :: String -> Binding -> Context -> Context
addbinding x bind ctx = (x, bind) : ctx

addname :: String -> Context -> Context
addname x ctx = (x, NameBind) : ctx

pickfreshname :: String -> Context -> (String, Context)
pickfreshname x ctx = case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind -> TyVarBind
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT -> TyAbbBind (typeShift d tyT)

getbinding :: Context -> Int -> Binding
getbinding ctx i = bindingShift (i + 1) (snd $ ctx !! i)

getTypeFromContext :: MonadThrow m => Context -> Int -> m Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> return tyT
        _ -> throwString $ "Wrong kind of binding for variable " ++ index2name ctx i

getVarIndex :: MonadFail m => String -> Context -> m Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> return i
        Nothing -> fail $ "Unbound variable name: '" ++ var ++ "'"

----------------------------------------------------------------
-- Ty
----------------------------------------------------------------
tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar x n -> onvar c x n
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyRecord fieldtys -> TyRecord (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)
                TyVariant fieldtys -> TyVariant (map (\(li, tyTs) -> (li, map (walk c) tyTs)) fieldtys)
                TyRec x tyT -> TyRec x (walk (c + 1) tyT)
                _ -> tyT

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                ( \c x n ->
                        if x < c
                                then TyVar x (n + d)
                                else TyVar (x + d) (n + d)
                )

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
        tymap
                ( \j x n ->
                        if x == j
                                then typeShift j tyS
                                else TyVar x n
                )

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmFix t1 -> TmFix (walk c t1)
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmCase t1 cases -> TmCase (walk c t1) (map (\(li, (ki, ti)) -> (li, (ki, walk (c + 1) ti))) cases)
                TmTag l ts1 tyT2 -> TmTag l (map (walk c) ts1) (ontype c tyT2)
                TmFold tyT -> TmFold (ontype c tyT)
                TmUnfold tyT -> TmUnfold (ontype c tyT)
                TmSucc t1 -> TmSucc (walk c t1)
                TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
                _ -> t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c x n ->
                        if x < c
                                then TmVar x (n + d)
                                else TmVar (x + d) (n + d)
                )
                (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                ( \j x n ->
                        if x == j
                                then termShift j s
                                else TmVar x n
                )
                (\j tyT -> tyT)
                j

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

----------------------------------------------------------------
-- Printing
----------------------------------------------------------------
printtm :: Context -> Term -> String
printtm ctx t = case t of
        TmVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TmAbs x tyT1 t2 ->
                let (x', ctx') = pickfreshname x ctx
                 in "(λ" ++ x' ++ ": " ++ printty ctx tyT1 ++ ". " ++ printtm ctx' t2 ++ ")"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
        TmFix t1 -> "fix " ++ printtm ctx t1
        TmRecord fields -> ""
        TmCase t1 cases -> "case " ++ printtm ctx t1 ++ " of {" ++ "}"
        TmTag l ts1 tyT2 -> l ++ " " ++ unwords (map (printtm ctx) ts1) ++ ": " ++ printty ctx tyT2
        TmFold tyT -> ""
        TmUnfold tyT -> ""
        TmSucc t1 -> "succ " ++ printtm ctx t
        TmZero -> "zero"
        TmTrue -> "true"
        TmFalse -> "false"
        TmIf t1 t2 t3 -> "if " ++ printtm ctx t1 ++ " then " ++ printtm ctx t2 ++ " else " ++ printtm ctx t3

printty :: Context -> Ty -> String
printty ctx ty = case ty of
        TyVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TyArr tyT1 tyT2 -> "(" ++ printty ctx tyT1 ++ " -> " ++ printty ctx tyT2 ++ ")"
        TyRecord fields -> ""
        TyVariant fields -> ""
        TyRec x tyT -> ""
        TyBool -> "Bool"
        TyNat -> "Nat"
