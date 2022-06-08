module FOmega.Syntax where

import Control.Monad.State (
        MonadState (state),
        State,
        StateT,
        modify,
        runState,
 )

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAbs String Kind Ty
        | TyAll String Kind Ty
        deriving (Eq, Show)

data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmTAbs String Kind Term
        | TmTApp Term Ty
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Ty
        | TyVarBind Kind
        | TyAbbBind Ty (Maybe Kind)
        | TmAbbBind Term (Maybe Ty)

type Context = [(String, Binding)]

data Command
        = Import String
        | Eval Term
        | Bind String Binding
        | SomeBind String String Term

addbinding :: String -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

pickfreshname :: Monad m => String -> StateT Context m String
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar x n -> onvar c x n
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)

tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)

----------------------------------------------------------------
-- Shift
----------------------------------------------------------------
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
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt
        TmAbbBind t tyT_opt ->
                let tyT_opt' = typeShift d <$> tyT_opt
                 in TmAbbBind (termShift d t) tyT_opt'

----------------------------------------------------------------
-- Substitution
----------------------------------------------------------------
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
