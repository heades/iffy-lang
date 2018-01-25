{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Repl where

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException    
import System.Exit
import Unbound.Generics.LocallyNameless.Subst
import Unbound.Generics.LocallyNameless
import qualified Data.Map.Strict as M

import Queue
import Grammar
import Lexer
import Pretty
import Eval
import Exp


type Qelm = ((Name Exp), Exp)
type REPLStateIO = StateT (Queue (QDefName, QDefDef)) IO

instance MonadException m => MonadException (StateT (Queue (QDefName, QDefDef)) m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'                       

data QDefName = RVar (Name Exp) | DefName (Name Exp)
    deriving (Show, Eq)
data QDefDef  = DefTerm Exp
    deriving Show


getQDefM :: (QDefName, QDefDef) -> REPLStateIO (Name Exp, Exp)
getQDefM e@(RVar x, DefTerm t) = return (x , t)
getQDefM e@(x,y) = error $ "Failed to get definition from context. Mismatched variable and type or term in: "++(prettyDef e)

getQDef :: (QDefName, QDefDef) -> (Name Exp, Exp)
getQDef e@(RVar x, DefTerm t) = (x , t)
getQDef e@(x,y) = error $ "Failed to get definition from context. Mismatched variable and type or term in: "++(prettyDef e)

-- Extract only closed terms from queue
getQCTM :: Queue (QDefName,QDefDef) -> Queue Qelm -> REPLStateIO (Queue Qelm)
getQCTM (Queue [] []) qCV = return $ qCV
getQCTM q qCV = getQDefM (headQ q) >>= (\x -> case x of 
                 cv -> getQCTM (tailQ q) (enqueue cv qCV))
                 

-- Extract only closed terms from queue, non-monadic version
getQCT :: Queue (QDefName,QDefDef) -> Queue Qelm -> (Queue Qelm)
getQCT (Queue [] []) qCV = qCV
getQCT q qCV = case getQDef (headQ q) of 
                 cv -> getQCT (tailQ q) (enqueue cv qCV)
                 
                
io :: IO a -> REPLStateIO a
io i = liftIO i

prettyDef :: (QDefName, QDefDef) -> String
prettyDef elem = let (a,t) = getQDef elem in "let "++(name2String a)++" = "++(runPP t)
    
pop :: REPLStateIO (QDefName, QDefDef)
pop = get >>= return.headQ

push :: (QDefName, QDefDef) -> REPLStateIO ()
push t = do
  q <- get
  put (q `snoc` t)

unfoldQueue :: (Queue Qelm) -> (Queue Qelm)
unfoldQueue q = fixQ q emptyQ step
 where
   step :: (Name Exp, Exp) -> t -> Queue Qelm -> Queue Qelm
   step e@(x,t) _ r = (mapQ (substDef x t) r) `snoc` e
    where
      substDef :: Name Exp -> Exp -> Qelm -> Qelm
      substDef x t (y, t') = (y, subst x t t')  
      
unfoldDefsInTerm :: (Queue Qelm) -> Exp -> Exp
unfoldDefsInTerm q t =
   let uq = toListQ $ unfoldQueue q
    in substs uq t

containsTerm_Qelm :: Queue Qelm -> QDefName -> Bool
containsTerm_Qelm (Queue f r) v@(RVar vnm) = ((foldl (\b (defName, defTerm)-> b || (vnm == defName)) False r) || (foldl (\b (defName, defTerm)-> b || (vnm == defName)) False  f ))
containsTerm_Qelm (Queue f r) v@(DefName vnm) = ((foldl (\b (defName, defTerm)-> b || (vnm == defName)) False r) || (foldl (\b (defName, defTerm)-> b || (vnm == defName)) False  f ))
      
containsTerm :: Queue (QDefName,QDefDef) -> QDefName -> Bool
containsTerm (Queue [] []) _ = False
containsTerm q v = (containsTerm_Qelm (getQCT q emptyQ) v) 




handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =    
   case (iffy $ alexScanTokens s) of
    l -> handleLine l
  where      
    handleLine (Let x t) = do
      defs' <- get
      defs <- getQCTM defs' emptyQ
      if(containsTerm defs' (RVar x))
      then io.putStrLn $ "error: The variable "++(show x)++" is already in the context."
      else push (RVar x,DefTerm t)
      
    handleLine (ShowAST t) = do
      defs' <- get
      defs <- getQCTM defs' emptyQ
      io.putStrLn.show $ unfoldDefsInTerm defs t

    handleLine (Unfold t) = do
      defs' <- get
      defs <- getQCTM defs' emptyQ
      io.putStrLn.runPP $ unfoldDefsInTerm defs t

    handleLine DumpState = get >>= io.print.(mapQ prettyDef)
    
    handleLine t = do
      defs' <- get
      defs <- getQCTM defs' emptyQ
      let tu = unfoldDefsInTerm defs t
          r = eval tu
       in io.putStrLn.runPP $ r
     
   
-- getFV :: Exp -> [Name Exp]
-- getFV t = fv t :: [Name Exp]

helpMenu :: String                          
helpMenu = 
      "-----------------------------------------------------------------------------------\n"++
      "                  The Iffy-Lang Help Menu                                         \n"++
      "-----------------------------------------------------------------------------------\n"++
      ":help             (:h)  Display the help menu\n"++
      ":quit             (:q)  Quit Iffy-Lang\n"++
      ":show <term>      (:s)  Display the Abstract Syntax Type of a term\n"++
      ":unfold <term>    (:u)  Unfold the expression into one without toplevel definition.\n"++ 
      ":dump             (:d)  Display the context\n"++
      "-----------------------------------------------------------------------------------"
          
repl :: IO ()
repl = do
  evalStateT (runInputT defaultSettings loop) emptyQ
   where 
       loop :: InputT REPLStateIO ()
       loop = do           
           minput <- getInputLine "Iffy> "
           case minput of
               Nothing -> return ()
               Just [] -> loop
               Just input | input == ":q" || input == ":quit"
                              -> liftIO $ putStrLn "Leaving Iffy." >> return ()
                          | input == ":h" || input == ":help"
                              -> (liftIO $ putStrLn helpMenu) >> loop                                 
                          | otherwise -> (lift.handleCMD $ input) >> loop