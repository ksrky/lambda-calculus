module Evaluator where

import Control.Monad.State
import Syntax

typeof :: Term -> State Context Ty
typeof t = case t of
        TmVar i _ -> do
                ctx <- get
                return $ getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ TyArr tyT1 tyT2
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
                        _ -> error "arrow type expected"
        TmTrue -> return TyBool
        TmFalse -> return TyBool
        TmIf t1 t2 t3 -> do
                tyT1 <- typeof t1
                if tyT1 == TyBool
                        then do
                                tyT2 <- typeof t2
                                tyT3 <- typeof t3
                                if tyT2 == tyT3 then return tyT2 else error "arms of conditional have different types"
                        else error "guard of conditional not a boolean"

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
        walk c t = case t of
                TmVar x n ->
                        if x >= c
                                then TmVar (x + d) (n + d)
                                else TmVar x (n + d)
                TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
                t -> t

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
    where
        walk c t = case t of
                TmVar x n -> if x == j + c then termShift c s else TmVar x n
                TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
                t -> t

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmTrue -> True
        TmFalse -> True
        _ -> False

eval1 :: Term -> Maybe Term
eval1 t = case t of
        TmApp (TmAbs x ty t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
        TmApp v1 t2 | isval v1 -> do
                t2' <- eval1 t2
                Just $ TmApp v1 t2'
        TmApp t1 t2 -> do
                t1' <- eval1 t1
                Just $ TmApp t1' t2
        TmIf TmTrue t2 t3 -> return t2
        TmIf TmFalse t2 t3 -> return t3
        TmIf t1 t2 t3 -> do
                t1' <- eval1 t1
                return $ TmIf t1' t2 t3
        _ -> Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
