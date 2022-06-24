module Syntax where

import Control.Monad.State
import qualified Data.Bifunctor

data Ty
        = TyId String
        | TyVar Int Int
        | TyUnit
        | TyFloat
        | TyRecord [(String, Ty)]
        | TyVariant [(String, Ty)]
        | TyString
        | TyBool
        | TyArr Ty Ty
        | TyNat
        deriving (Eq, Show)

data Term
        = TmAscribe Term Ty
        | TmString String
        | TmTrue
        | TmFalse
        | TmIf Term Term Term
        | TmCase Term [(String, (String, Term))]
        | TmTag String Term Ty
        | TmUnit
        | TmVar Int Int
        | TmFloat Float
        | TmTimesfloat Term Term
        | TmLet String Term Term
        | TmRecord [(String, Term)]
        | TmProj Term String
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmFix Term
        | TmZero
        | TmSucc Term
        | TmPred Term
        | TmIsZero Term
        | TmInert Ty
        deriving (Eq, Show)

data Binding
        = NameBind
        | TmAbbBind Term (Maybe Ty)
        | VarBind Ty
        | TyVarBind
        | TyAbbBind Ty
        deriving (Eq, Show)

type Context = [(String, Binding)]

data Command
        = Import String
        | Eval Term
        | Bind String Binding
        deriving (Eq, Show)

addbinding :: String -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

addname :: String -> State Context ()
addname x = addbinding x NameBind

pickfreshname :: Monad m => String -> StateT Context m String
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

name2index :: Context -> String -> Int
name2index ctx x = case ctx of
        [] -> error $ "Identifier " ++ x ++ " is unbound"
        (y, _) : rest ->
                if y == x
                        then 0
                        else 1 + name2index rest x

tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyString -> TyString
                TyId b -> tyT
                TyVariant fieldtys -> TyVariant $ map (Data.Bifunctor.second (walk c)) fieldtys
                TyUnit -> TyUnit
                TyFloat -> TyFloat
                TyRecord fieldtys -> TyRecord $ map (Data.Bifunctor.second (walk c)) fieldtys
                TyVar x n -> onvar c x n
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyBool -> TyBool
                TyNat -> TyNat

tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmAscribe t1 tyT1 -> TmAscribe (walk c t1) (ontype c tyT1)
                TmString s -> TmString s
                TmVar x n -> onvar c x n
                TmTrue -> TmTrue
                TmFalse -> TmFalse
                TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
                TmTag l t1 tyT -> TmTag l (walk c t1) (ontype c tyT)
                TmCase t cases -> TmCase (walk c t) (map (\(li, (xi, ti)) -> (li, (xi, walk (c + 1) ti))) cases)
                TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
                TmUnit -> TmUnit
                TmInert tyT -> TmInert (ontype c tyT)
                TmFloat n -> TmFloat n
                TmTimesfloat t1 t2 -> TmTimesfloat (walk c t1) (walk c t2)
                TmProj t1 l -> TmProj (walk c t1) l
                TmRecord fields -> TmRecord $ map (Data.Bifunctor.second (walk c)) fields
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmFix t1 -> TmFix (walk c t1)
                TmZero -> TmZero
                TmSucc t1 -> TmSucc (walk c t1)
                TmPred t1 -> TmPred (walk c t1)
                TmIsZero t1 -> TmIsZero (walk c t1)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                (\c x n -> if x >= c then TyVar (x + d) (n + d) else TyVar x (n + d))

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

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

bindingshift :: Int -> Binding -> Binding
bindingshift d bind = case bind of
        NameBind -> NameBind
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind -> TyVarBind
        TyAbbBind tyT -> TyAbbBind (typeShift d tyT)

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                (\j x n -> if x == j then termShift j s else TmVar x n)
                (\j tyT -> tyT)
                j

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
        tymap
                (\j x n -> if x == j then typeShift j tyS else TyVar x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

tytermSubst :: Ty -> Int -> Term -> Term
tytermSubst tyS =
        tmmap
                (\c x n -> TmVar x n)
                (typeSubst tyS)

tytermSubstTop :: Ty -> Term -> Term
tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

getbinding :: Context -> Int -> Binding
getbinding ctx i =
        if i < length ctx
                then
                        let (_, bind) = ctx !! i
                         in bindingshift (i + 1) bind
                else error $ "Variable lookup failure: offset: " ++ show i ++ "ctx size: " ++ show (length ctx)

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case getbinding ctx i of
        VarBind tyT -> tyT
        TmAbbBind _ (Just tyT) -> tyT
        TmAbbBind _ Nothing -> error $ "No type recorded for variable " ++ index2name ctx i
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i
