module Syntax where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyRecord [(String, Ty)]
        | TyVariant [(String, Ty)]
        | TyUnit
        deriving (Eq, Show)

data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmLet String Term Term
        | TmFix Term
        | TmRecord [(String, Term)]
        | TmProj Term String
        | TmCase Term [(String, (String, Term))]
        | TmTag String Term Ty
        | TmUnit
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
getTypeFromContext ctx i = case getbinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> throwString $ "No type recorded for variable " ++ index2name ctx i
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
                TyUnit -> TyUnit

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
                TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
                TmFix t1 -> TmFix (walk c t1)
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmProj t1 l -> TmProj (walk c t1) l
                TmCase t1 cases -> TmCase (walk c t1) (map (\(li, (xi, ti)) -> (li, (xi, walk (c + 1) ti))) cases)
                TmTag l t1 tyT2 -> TmTag l (walk c t1) (ontype c tyT2)
                TmUnit -> TmUnit

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
outer :: Bool -> String -> String
outer True s = "(" ++ s ++ ")"
outer False s = s

printtm :: Context -> Bool -> Term -> String
printtm ctx b t = case t of
        TmVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TmAbs x tyT1 t2 ->
                let (x', ctx') = pickfreshname x ctx
                 in outer b $ "λ" ++ x' ++ ": " ++ printty ctx False tyT1 ++ ". " ++ printtm ctx' False t2
        TmApp t1 t2 -> outer b $ printtm ctx False t1 ++ " " ++ printtm ctx True t2
        TmLet x t1 t2 ->
                let (x', ctx') = pickfreshname x ctx
                 in outer b $ "let " ++ x ++ " = " ++ printtm ctx False t1 ++ " in " ++ printtm ctx' False t2
        TmFix t1 -> outer b $ "fix " ++ printtm ctx True t1
        TmRecord fields ->
                let pf i (li, ti) =
                        (if show li /= show i then li ++ "=" else "") ++ printtm ctx False ti
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                 in "{" ++ pfs 1 fields ++ "}"
        TmProj t1 l -> printtm ctx False t1 ++ "." ++ l
        TmCase t1 cases ->
                let palt (li, (xi, ti)) =
                        let (xi', ctx') = pickfreshname xi ctx
                         in li ++ " " ++ xi ++ " -> " ++ printtm ctx' False ti
                    palts ctx [] = ""
                    palts ctx [a] = palt a
                    palts ctx (a : rest) = palt a ++ " | " ++ palts ctx rest
                 in outer b $ "case " ++ printtm ctx False t1 ++ " of {" ++ palts ctx cases ++ "}"
        TmTag l t1 tyT2 -> "<" ++ l ++ "=" ++ printtm ctx True t1 ++ "> as " ++ printty ctx False tyT2
        TmUnit -> "()"

printty :: Context -> Bool -> Ty -> String
printty ctx b ty = case ty of
        TyVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TyArr tyT1 tyT2 -> outer b $ printty ctx True tyT1 ++ " -> " ++ printty ctx False tyT2
        TyRecord fields ->
                let pf i (li, tyTi) =
                        (if show li /= show i then li ++ "=" else "") ++ printty ctx False tyTi
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                 in "{" ++ pfs 1 fields ++ "}"
        TyVariant fields ->
                let pf i (li, tyTi) = li ++ ": " ++ printty ctx False tyTi
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ " | " ++ pfs (i + 1) rest
                 in "<" ++ pfs 1 fields ++ ">"
        TyUnit -> "()"
