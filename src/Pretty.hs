{-# LANGUAGE MultiParamTypeClasses
    , ScopedTypeVariables
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , DeriveGeneric
#-}
module Pretty where
import Exp
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Alpha
import GHC.Generics (Generic)

prettyPrinter :: Exp -> LFreshM String
prettyPrinter Zero = return "0"
prettyPrinter One  = return "1"

prettyPrinter (Fun b) = do
    lunbind b $(\(x,t) -> do
        ts <- prettyPrinter t
        return $ "("++"fun "++(name2String x)++" => "++ts++")")
        --return $ show t++x)
        
prettyPrinter (Or x y) = do
    a <- prettyPrinter x
    b <- prettyPrinter y
    return $ a++" or "++b

prettyPrinter (And x y) = do
    a <- prettyPrinter x
    b <- prettyPrinter y
    return $ a++" and "++b

prettyPrinter (App x y) = do
    a <- prettyPrinter x
    b <- prettyPrinter y
    return $ "app "++a++" to "++b
        
prettyPrinter (Var x) = return $ name2String x    
prettyPrinter b = return $ show b

runPP :: Exp -> String
runPP = runLFreshM.prettyPrinter


