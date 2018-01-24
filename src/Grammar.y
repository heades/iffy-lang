{

module Grammar where
import Lexer
import Exp
import Unbound.Generics.LocallyNameless
}


%name iffy
%tokentype { Token }
%error { parseError }
%token
    if                                      { TokenIf }
    then                                    { TokenThen }
    else                                    { TokenElse }
    '&'                                     { TokenAnd }
    '|'                                     { TokenOr }
    '0'                                     { TokenZero }
    '1'                                     { TokenOne }
    fun                                     { TokenFun }
    var                                     { TokenVar $$ }
    app                                     { TokenApp }
    to                                      { TokenTo }
    '=>'                                    { TokenFunIs }
    '('                                     { TokenLeftPer }
    ')'                                     { TokenRightPer }
    let                                     { TokenLet }
    '='                                     { TokenEqual }
    ':d'                                    { TokenDump }
    ':s'                                    { TokenShowAST }
    ':u'                                    { TokenUnFold }

%right var
%right else
%right app
%left '|'
%left '&'

%%
Exp :: { Exp }
Exp     : if Exp then Exp else Exp       { If $2 $4 $6 }
        | let var '=' Exp                { Let (s2n $2) $4 }
        | app Exp to Exp                 { App $2 $4 }
        | '('fun var '=>' Exp')'         { Fun $ bind (s2n $3) $5 }
		| Exp '&' Exp                    { And $1 $3 }
        | Exp '|' Exp                    { Or $1 $3 }
		| '0'                            { Zero }
        | '1'                            { One }
        | var                            { Var (s2n $1) }
        | ':d'                           { DumpState }
        | ':s' Exp                       { ShowAST $2 }
        | ':u' Exp                       { Unfold $2 }
        
		
{
parseError :: [Token] -> a
parseError _ = error "Parse error"


         

}