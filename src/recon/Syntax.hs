module Syntax where

{-
import Control.Exception.Safe
import Data.List

data Ty
        = TyArr Ty Ty
        | TyId String
        | TyBool
        | TyNat

data Term
        = TmVar Int Int
        | TmAbs String (Maybe Ty) Term
        | TmApp Term Term
        | TmLet String Term Term

data Binding
        = NameBind
        | VarBind Ty

data Command
        = Eval Term
        | Bind String Binding

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

getbinding :: Context -> Int -> Binding
getbinding ctx i = bindingShift (i + 1) (snd $ ctx !! i)

getTypeFromContext :: MonadThrow m => Context -> Int -> m Ty
getTypeFromContext ctx i = case getbinding ctx i of
        VarBind tyT -> return tyT
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
                TyVariant fieldtys -> TyVariant (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)
                TyRec x tyT -> TyRec x (walk c tyT)

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
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmProj t1 l -> TmProj (walk c t1) l
                TmCase t1 cases -> TmCase (walk c t1) (map (\(li, (xi, ti)) -> (li, (xi, walk (c + 1) ti))) cases)
                TmTag l t1 tyT2 -> TmTag l (walk c t1) (ontype c tyT2)
                TmFold tyT -> TmFold (ontype c tyT)
                TmUnfold tyT -> TmUnfold (ontype c tyT)

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
-}