{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax where

import Control.Monad.Identity (Identity)
import Control.Monad.State (
        MonadState (get, state),
        State,
        StateT,
        runState,
 )

data Term
        = TmVar Int Int
        | TmAbs String Term
        | TmApp Term Term
        deriving (Show)

type Context = [(String, Binding)]

data Binding = NameBind

printtm :: Term -> State Context String
printtm t = case t of
        TmAbs x t1 -> do
                x' <- pickfreshname x
                t1' <- printtm t1
                return $ "(λ" ++ x' ++ ". " ++ t1' ++ ")"
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

pickfreshname :: Monad m => String -> StateT Context m String
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)
