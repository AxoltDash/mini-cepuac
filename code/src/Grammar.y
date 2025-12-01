{
module Grammar where
import Lexer
import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var        { TokenId $$ }
      double     { TokenNum $$ }
      "#t"      { TokenBool True }
      "#f"      { TokenBool False }
      ':'       { TokenColon }
      '('       { TokenLParen }
      ')'       { TokenRParen }
      '+'       { TokenPlus }
      '-'       { TokenMinus }
      '/'       { TokenSlash }
      '*'       { TokenMul }
      "&&"      { TokenAnd }
      "||"      { TokenOr }
      "not"     { TokenNot }
      "let"     { TokenLet }
      "boolean" { TokenBoolean }
      "number"  { TokenNumber }
      "lambda"  { TokenLambda }
      "->"      { TokenArrow }
      '{'       { TokenLCurly }
      '|'       { TokenPipe }
      '}'       { TokenRCurly }
      "=="      { TokenEqEq }
      "!="      { TokenNeq }
      '>'       { TokenGT }
      ">="      { TokenGE }
%%

ASA :  var { Id $1 }
     | double { Num $1 }
     | "#t" { ABool True }
     | "#f" { ABool False }
     | '(' '+' ASA ASA ')' { Add $3 $4 }
     | '(' '-' ASA ASA ')' { Sub $3 $4 }
     | '(' '*' ASA ASA ')' { Mul $3 $4 }
     | '(' '/' ASA ASA ')' { Div $3 $4 }
     | '(' "&&" ASA ASA ')' { And $3 $4 }
     | '(' "||" ASA ASA ')' { Or $3 $4 }
     | '(' "not" ASA ')' { Not $3 }
     | '(' "lambda" ':' Type "->" Type '(' var ')' ASA ')' { Lambda (Arrow $4 $6) $8 $10 }
     | '(' "let" '(' var ':' Type ASA ')' ASA ')' { Let ($4, $6) $7 $9 }
     | '(' ASA ASA ')' { App $2 $3 }

Type: "number" { Refinement "v" Number MaybeZero }
    | "boolean" { Refinement "v" Boolean NonZero }
    | '{' var ':' Type '|' Predicate '}' { Refinement $2 $4 $6 }
    | '(' Type "->" Type ')' { Arrow $2 $4 }

Predicate: var "==" double { if $3 == 0 then Zero $3 else parseError [] }
         | var "!=" double { if $3 == 0 then NonZero $3 else parseError [] }
         | var '>' double { if $3 == 0 then NonZero $3 else parseError [] }
         | var ">=" double { if $3 == 0 then MaybeZero $3 else parseError [] }
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Type
  = Boolean
  | Number

data RefinementType =  Refinement String Type Predicate

data Predicate 
  = NonZero
  | Zero 
  | MaybeZero
  deriving (Show, Eq)

data ASA
  = ANum Double
  | ABool Bool
  | Id String
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | And ASA ASA
  | Or ASA ASA
  | Not ASA
  | Lambda RefinementType String ASA
  | App ASA ASA
  | Let (String, RefinementType) ASA ASA
  deriving (Show, Eq)

main = getContents >>= print . parse . lexer

}
