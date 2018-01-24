module Main where

import Lib
import Grammar
import Lexer
import System.IO
import Exp
import Eval
import Pretty

main :: IO()         
main = do      
 hSetBuffering stdout NoBuffering   
 putStr "IFFY> "
 s <- getLine
 if s == ""
 then 
    main
 else 
    print $ runPP $ eval $ iffy (alexScanTokens s)
    --print $ eval $ iffy (alexScanTokens s)    
 main
