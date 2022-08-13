module LambdaPi.Syntax where

import Control.Exception.Safe
import Control.Monad.State
import Data.List

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmPi String Ty Ty
        deriving (Eq, Show)

data Ty
        = TyTerm Term
        | TyStar
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Ty
        | TmAbbBind Term (Maybe Ty)
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
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)

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
-- Type
----------------------------------------------------------------
tymap :: (Int -> Term -> Term) -> Int -> Ty -> Ty
tymap onterm c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyTerm t -> TyTerm (onterm c t)
                TyStar -> TyStar

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d = tymap (termShiftAbove d)

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x t1 t2 -> TmAbs x (ontype c t1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmPi x tyT1 tyT2 -> TmPi x (ontype c tyT1) (ontype (c + 1) tyT2)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c x n ->
                        if x >= c
                                then TmVar (x + d) (n + d)
                                else TmVar x (n + d)
                )
                (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                (\j x n -> if x == j then termShift j s else TmVar x n)
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
        TmPi x t1 t2 ->
                let (x', ctx') = pickfreshname x ctx
                 in "(∀" ++ x' ++ ": " ++ printty ctx t1 ++ ". " ++ printty ctx' t2 ++ ")"

printty :: Context -> Ty -> String
printty ctx tyT = case tyT of
        TyTerm t -> printtm ctx t
        TyStar -> "*"
