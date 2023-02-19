module LambdaPi.Syntax where

import Data.List (elemIndex)

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs String Type Term
        deriving (Eq, Show)

data Type
        = TyVar Int Int
        | TyApp Type Term
        | TyPi String Type Type
        deriving (Eq, Show)

data Kind
        = KnStar
        | KnPi String Type Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Type
        | TyVarBind Kind
        | TmAbbBind Term (Maybe Type)
        | TyAbbBind Type (Maybe Kind)
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
        TyVarBind knK -> TyVarBind (kindShift d knK)
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT knK_opt -> TyAbbBind (typeShift d tyT) (kindShift d <$> knK_opt)

getBinding :: Context -> Int -> Binding
getBinding ctx i = bindingShift (i + 1) (snd $ ctx !! i)

getType :: MonadFail m => Context -> Int -> m Type
getType ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> fail $ "No type recorded for variable " ++ index2name ctx i
        _ -> fail $ "Wrong kind of binding for variable " ++ index2name ctx i

getKind :: MonadFail m => Context -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ (Just knK) -> return knK
        _ -> fail $ "Wrong kind of binding for type " ++ index2name ctx i

getVarIndex :: MonadFail m => String -> Context -> m Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> return i
        Nothing -> fail $ "Unbound variable name: '" ++ var ++ "'"

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmAbs x t1 t2 -> TmAbs x (ontype c t1) (walk (c + 1) t2)

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

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
tymap :: (Int -> Int -> Int -> Type) -> (Int -> Term -> Term) -> Int -> Type -> Type
tymap onvar onterm c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar x n -> onvar c x n
                TyApp tyT1 t2 -> TyApp (walk c tyT1) (onterm c t2)
                TyPi x tyT1 tyT2 -> TyPi x (walk c tyT1) (walk c tyT2)

typeShiftAbove :: Int -> Int -> Type -> Type
typeShiftAbove d =
        tymap
                ( \c x n ->
                        if x < c
                                then TyVar x (n + d)
                                else TyVar (x + d) (n + d)
                )
                (termShiftAbove d)

typeShift :: Int -> Type -> Type
typeShift d = typeShiftAbove d 0

termtySubst :: Term -> Int -> Type -> Type
termtySubst s =
        tymap
                (\_ x n -> TyVar x n)
                (termSubst s)

termtySubstTop :: Term -> Type -> Type
termtySubstTop s tyT = typeShift (-1) (termtySubst (termShift 1 s) 0 tyT)

----------------------------------------------------------------
-- Kind
----------------------------------------------------------------
knmap :: (Int -> Type -> Type) -> Int -> Kind -> Kind
knmap ontype c knK = walk c knK
    where
        walk c knK = case knK of
                KnStar -> KnStar
                KnPi x tyT1 knK2 -> KnPi x (ontype c tyT1) (walk c knK2)

kindShiftAbove :: Int -> Int -> Kind -> Kind
kindShiftAbove d = knmap (typeShiftAbove d)

kindShift :: Int -> Kind -> Kind
kindShift d = kindShiftAbove d 0

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

printty :: Context -> Type -> String
printty ctx ty = case ty of
        TyVar x n ->
                if length ctx == n
                        then index2name ctx x
                        else "[bad index]"
        TyApp tyT1 t2 -> "(" ++ printty ctx tyT1 ++ " " ++ printtm ctx t2 ++ ")"
        TyPi x tyT1 tyT2 ->
                let (x', ctx') = pickFreshname x ctx
                 in "(Π" ++ x' ++ ":" ++ printty ctx tyT1 ++ ". " ++ printty ctx' tyT2 ++ ")"

printkn :: Context -> Kind -> String
printkn ctx knK = case knK of
        KnStar -> "*"
        KnPi x tyT1 knK2 ->
                let (x', ctx') = pickFreshname x ctx
                 in "(Π" ++ x' ++ ":" ++ printty ctx tyT1 ++ ". " ++ printkn ctx' knK2 ++ ")"

printbind :: Context -> (String, Binding) -> String
printbind _ (x, NameBind) = x ++ ": -"
printbind ctx (x, VarBind tyT) = x ++ ": " ++ printty ctx tyT
printbind ctx (x, TmAbbBind _ (Just tyT)) = x ++ ": " ++ printty ctx tyT
printbind _ (x, TmAbbBind _ Nothing) = x ++ ": -"
printbind ctx (x, TyVarBind knK) = x ++ ": " ++ printkn ctx knK
printbind ctx (x, TyAbbBind _ (Just knK)) = x ++ ": " ++ printkn ctx knK
printbind _ (x, TyAbbBind _ Nothing) = x ++ ": -"