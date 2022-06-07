{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typed.Syntax where

import Control.Monad.State (
        MonadState (get, state),
        State,
        StateT,
        modify,
        runState,
 )

data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmTrue
        | TmFalse
        | TmIf Term Term Term
        deriving (Show)

data Ty = TyArr Ty Ty | TyBool deriving (Eq, Show)

type Context = [(String, Binding)]

data Binding = NameBind | VarBind Ty

addbinding :: String -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> tyT
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i

printtm :: Term -> State Context String
printtm t = case t of
        TmAbs x ty t1 -> do
                x' <- pickfreshname x
                t1' <- printtm t1
                return $ "(lambda " ++ x' ++ ": " ++ show ty ++ ". " ++ t1' ++ ")"
        TmApp t1 t2 -> do
                t1' <- printtm t1
                t2' <- printtm t2
                return $ "(" ++ t1' ++ t2' ++ ")"
        TmVar x n -> do
                ctx <- get
                return $
                        if length ctx == n
                                then index2name ctx x
                                else "[bad index]"
        TmTrue -> return "true"
        TmFalse -> return "false"
        TmIf t1 t2 t3 -> do
                t1' <- printtm t1
                t2' <- printtm t2
                t3' <- printtm t3
                return $ "if " ++ t1' ++ " then " ++ t2' ++ " else " ++ t3'

pickfreshname :: Monad m => String -> StateT Context m String
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)
