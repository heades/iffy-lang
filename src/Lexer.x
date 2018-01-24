{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                     ;
    if                          { \s -> TokenIf }
    then                        { \s -> TokenThen }
    else                        { \s -> TokenElse }
    fun                         { \s -> TokenFun }
    "=>"                        { \s -> TokenFunIs }
    app                         { \s -> TokenApp }
    to                          { \s -> TokenTo }
    let                         { \s -> TokenLet }
    ":s"                        { \s -> TokenShowAST }
    ":u"                        { \s -> TokenUnFold }
    ":d"                        { \s -> TokenDump }
    $alpha [$alpha $digit]*     { \s -> TokenVar s}
    &                           { \s -> TokenAnd }
    \|                          { \s -> TokenOr }
    0                           { \s -> TokenZero }
    1                           { \s -> TokenOne }
    \(                          { \s -> TokenLeftPer }
    \)                          { \s -> TokenRightPer }
    \=                          { \s -> TokenEqual }
    
    
    
{
data Token = TokenIf
           | TokenVar String
           | TokenAnd
           | TokenOr
           | TokenThen
           | TokenElse
           | TokenZero
           | TokenOne
           | TokenFun
           | TokenApp
           | TokenFunIs
           | TokenTo
           | TokenLeftPer
           | TokenRightPer
           | TokenEqual
           | TokenLet
           | TokenShowAST
           | TokenUnFold
           | TokenDump
           deriving Show
           

}
