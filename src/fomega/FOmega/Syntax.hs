module FOmega.Syntax where

import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs String Ty Term
        | TmTApp Term Ty
        | TmTAbs String Kind Term
        deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAll String Kind Ty
        | TyApp Ty Ty
        | TyAbs String Kind Ty
        deriving (Eq, Show)

data Kind
        = KnStar
        | KnArr Kind Kind
        deriving (Eq, Show)
data Binding
        = NameBind
        | VarBind Ty
        | TyVarBind Kind
        | TmAbbBind Term (Maybe Ty)
        | TyAbbBind Ty (Maybe Kind)
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

addBinding :: String -> Binding -> Context -> Context
addBinding x bind ctx = (x, bind) : ctx

addName :: String -> Context -> Context
addName x ctx = (x, NameBind) : ctx

pickFreshname :: String -> Context -> (String, Context)
pickFreshname x ctx = case lookup x ctx of
        Just _ -> pickFreshname (x ++ "'") ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt

getBinding :: Context -> Int -> Binding
getBinding ctx i = bindingShift (i + 1) (snd $ ctx !! i)

getType :: MonadFail m => Context -> Int -> m Ty
getType ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> fail $ "No type recorded for variable " ++ index2name ctx i
        _ -> fail $ "Wrong kind of binding for variable " ++ index2name ctx i

getKind :: MonadFail m => Context -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ (Just knK) -> return knK
        TyAbbBind _ Nothing -> fail $ "No kind recorded for variable " ++ index2name ctx i
        _ -> fail $ "getkind: Wrong kind of binding for variable " ++ index2name ctx i

getVarIndex :: MonadFail m => String -> Context -> m Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> return i
        Nothing -> fail $ "Unbound variable name: '" ++ var ++ "'"

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)

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

termSubst :: Term -> Int -> Term -> Term
termSubst s =
        tmmap
                ( \j x n ->
                        if x == j
                                then termShift j s
                                else TmVar x n
                )
                (\_ tyT -> tyT)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst (termShift 1 s) 0 t)

tytermSubst :: Ty -> Int -> Term -> Term
tytermSubst tyS =
        tmmap
                (\_ x n -> TmVar x n)
                (typeSubst tyS)

tytermSubstTop :: Ty -> Term -> Term
tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar x n -> onvar c x n
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)

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
                (\j x n -> if x == j then typeShift j tyS else TyVar x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

----------------------------------------------------------------
-- Printing
----------------------------------------------------------------
printtm :: Context -> Term -> String
printtm ctx t = case t of
        TmVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TmApp t1 t2 -> "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
        TmAbs x tyT1 t2 ->
                let (x', ctx') = pickFreshname x ctx
                 in "(λ" ++ x' ++ ": " ++ printty ctx tyT1 ++ ". " ++ printtm ctx' t2 ++ ")"
        TmTApp t1 tyT2 -> "(" ++ printtm ctx t1 ++ " [" ++ printty ctx tyT2 ++ "]" ++ ")"
        TmTAbs tyX knK1 t2 ->
                let (tyX', ctx') = pickFreshname tyX ctx
                 in "(Λ" ++ tyX' ++ ": " ++ printkn knK1 ++ ". " ++ printtm ctx' t2 ++ ")"

printty :: Context -> Ty -> String
printty ctx ty = case ty of
        TyVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TyArr tyT1 tyT2 -> "(" ++ printty ctx tyT1 ++ " -> " ++ printty ctx tyT2 ++ ")"
        TyAll tyX knK1 tyT2 ->
                let (tyX', ctx') = pickFreshname tyX ctx
                 in "(∀" ++ tyX' ++ ": " ++ printkn knK1 ++ ". " ++ printty ctx' tyT2 ++ ")"
        TyApp tyT1 tyT2 -> "(" ++ printty ctx tyT1 ++ " " ++ printty ctx tyT2 ++ ")"
        TyAbs tyX knK1 tyT2 ->
                let (tyX', ctx') = pickFreshname tyX ctx
                 in "(λ" ++ tyX' ++ ": " ++ printkn knK1 ++ ". " ++ printty ctx' tyT2 ++ ")"

printkn :: Kind -> String
printkn kn = case kn of
        KnStar -> "*"
        KnArr knK1 knK2 -> "(" ++ printkn knK1 ++ " -> " ++ printkn knK2 ++ ")"
