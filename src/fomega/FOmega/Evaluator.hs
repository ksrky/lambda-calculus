module FOmega.Evaluator where

import FOmega.Syntax (
        Term (TmAbs, TmApp, TmTAbs, TmTApp),
        termSubstTop,
        tytermSubstTop,
 )

isval :: Term -> Bool
isval t = case t of
        TmAbs{} -> True
        TmTAbs{} -> True
        _ -> False

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

eval1 :: Term -> Maybe Term
eval1 t = case t of
        TmApp (TmAbs x ty t12) v2 | isval v2 -> return $ termSubstTop v2 t12
        TmApp v1 t2 | isval v1 -> do
                t2' <- eval1 t2
                return $ TmApp v1 t2'
        TmApp t1 t2 -> do
                t1' <- eval1 t1
                return $ TmApp t1' t2
        TmTApp (TmTAbs x _ t11) tyT2 -> return $ tytermSubstTop tyT2 t11
        TmTApp t1 tyT2 -> do
                t1' <- eval1 t1
                return $ TmTApp t1' tyT2
        _ -> Nothing
