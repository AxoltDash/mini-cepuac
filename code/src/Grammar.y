{
module Grammar where
import Lexer
import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var			  { TokenId $$ }
	  double          { TokenNum $$ }
      "#t"            { TokenBool True }
      "#f"            { TokenBool False }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '/'             { TokenSlash }
      '*'             { TokenMul }
      "&&"            { TokenAnd }
      "||"            { TokenOr }
      "not"           { TokenNot }
	  let			  { TokenLet }
%%

ASA :  var													{ Id $1 }
	 | double                                               { Num $1 }
     | "#t"                                                 { Boolean True }
     | "#f"                                                 { Boolean False }
     | '(' '+' ASA ASA ')'                                  { Add $3 $4}
     | '(' '-' ASA ASA ')'                                  { Sub $3 $4}
     | '(' '*' ASA ASA ')'									{ Mul $3 $4}
     | '(' '/' ASA ASA ')'                                  { Div $3 $4}
     | '(' "&&" ASA ')'										{ And $3 $4}
     | '(' "||" ASA ')'										{ Or $3 $4}
     | '(' "not" ASA ')'                                    { Not $3}
	 | '(' let '(' var SASA ')' SASA ')'					{ LetS $4 $5 $7 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA
  = Num Double
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | And ASA ASA
  | Or ASA ASA
  | Not ASA
  | Let String ASA ASA
  deriving (Show, Eq)

main = getContents >>= print . parse . lexer

}
