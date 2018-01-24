{-# LANGUAGE MultiParamTypeClasses
    , TemplateHaskell
    , ScopedTypeVariables
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , DeriveGeneric
#-}
module Eval where
import Exp
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Alpha
import GHC.Generics (Generic)

eval :: Exp -> Exp
eval t = runLFreshM $ eval' t


eval' :: Exp -> LFreshM Exp
eval' (App (Fun y) a2) = do
    lunbind y $ (\(x, body) -> eval' $ subst x a2 body)

eval' (And a1 a2) = do
                b1 <- eval' a1
                b2 <- eval' a2
                case (b1,b2) of
                    (One,One)   -> return One
                    (Zero,One)  -> return Zero
                    (One,Zero)  -> return Zero
                    (Zero,Zero) -> return Zero
                    (_,_)       -> return $ And b1 b2
                    
eval' (Or a1 a2) = do
                b1 <- eval' a1
                b2 <- eval' a2
                case (b1,b2) of
                    (One,One)   -> return One
                    (One,Zero)  -> return One
                    (Zero,One)  -> return One
                    (Zero,Zero) -> return Zero
                    (_,_)       -> return $ Or b1 b2

eval' (If a1 a2 a3) = do
                b1 <- eval' a1
                b2 <- eval' a2
                b3 <- eval' a3
                case b1 of
                    One  -> return b2
                    Zero -> return b3
                    _    -> return (If b1 b2 b3)
                    
eval' b = return b                    

