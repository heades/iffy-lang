{-# LANGUAGE MultiParamTypeClasses
    , TemplateHaskell
    , ScopedTypeVariables
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , DeriveGeneric
#-}
module Exp where
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Alpha
import GHC.Generics (Generic)


data Exp = If Exp Exp Exp
        | Let (Name Exp) Exp
        | And Exp Exp
        | Or Exp Exp
        | Zero
        | One
        | Fun (Bind (Name Exp) Exp)
        | App Exp Exp
        | Var (Name Exp)
        | DumpState
        | ShowAST Exp
        | Unfold Exp
         deriving (Show, Generic)
         
         


instance Alpha Exp

instance Subst Exp Exp where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing         