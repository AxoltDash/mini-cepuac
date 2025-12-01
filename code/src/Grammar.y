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
     | "#t" { Boolean True }
     | "#f" { Boolean False }
     | '(' '+' ASA ASA ')' { Add $3 $4 }
     | '(' '-' ASA ASA ')' { Sub $3 $4 }
     | '(' '*' ASA ASA ')' { Mul $3 $4 }
     | '(' '/' ASA ASA ')' { Div $3 $4 }
     | '(' "&&" ASA ASA ')' { And $3 $4 }
     | '(' "||" ASA ASA ')' { Or $3 $4 }
     | '(' "not" ASA ')' { Not $3 }
     | '(' "lambda" ':' Type "->" Type '(' var ')' ASA ')' { Lambda (Arrow $4 $6) $8 $10 }
     | '(' "let" '(' var ':' Type ASA ')' ASA ')' { Let ($4, $6) $7 $9 }
     | '(' ASA ASA ')' { App $2 $3}

Type: "number" { Number }
    | "boolean" { Bool }
    | '{' var ':' Type '|' Predicate '}' { Refinement $2 $4 $6 }
    | '(' Type "->" Type ')' { Arrow $2 $4 }

Predicate: var "==" double { PEq $3 }
         | var "!=" double { PNeq $3 }
         | var '>' double { PGT $3 }
         | var ">=" double { PGe $3 }
         | Predicate "&&" Predicate { PAnd $1 $3 }
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Type 
  = Bool 
  | Number
  | Arrow Type Type
  | Refinement String Type Predicate
  deriving(Show, Eq)

data Predicate
  = PNeq Double      
  | PEq Double      
  | PGT Double        
  | PGe Double       
  | PAnd Predicate Predicate
  deriving (Eq, Show)

data ASA
  = Num Double
  | Boolean Bool
  | Id String
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | And ASA ASA
  | Or ASA ASA
  | Not ASA
  | Lambda Type String ASA
  | App ASA ASA
  | Let (String, Type) ASA ASA
  deriving (Show, Eq)

main = getContents >>= print . parse . lexer

}
