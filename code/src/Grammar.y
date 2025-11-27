{
module Grammar where
import Lexer
import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenVar $$ }
      double          { TokenNum $$ }
      "#t"            { TokenBool True }
      "#f"            { TokenBool False }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '['             { TokenLBracket }
      ']'             { TokenRBracket }
      ','             { TokenComma }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '/'             { TokenSlash }
      ':'             {TokenAppend}
      "&&"            { TokenAnd }
      "||"            { TokenOr }
      '*'             { TokenMul }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLTE }
      ">="            { TokenGTE }
      "!="            { TokenNEQ }
      '='             { TokenEQ }
      "++"            { TokenInc }
      "--"            { TokenDec }
      "sqrt"          { TokenSqrt }
      "**"            { TokenPow }
      "fst"           { TokenFst }
      "snd"           { TokenSnd }
      "head"          { TokenHead }
      "not"           { TokenNot }
      "tail"          { TokenTail }
      "if"            { TokenIf }
      "let"           { TokenLetp }
      "let*"          { TokenLets }
      "letrec"        { TokenLetr }
      "lambda"        { TokenLambda }
      "else"          { TokenElse }
      "cond"          { TokenCond }
      "concat"        {TokenConcat}
%%

SASA : var                                                     { VarSrc $1 }
     | double                                                  { NumSrc $1 }
     | "#t"                                                    { BooleanSrc True }
     | "#f"                                                    { BooleanSrc False }
     | '(' SASA ',' SASA ')'                                   { PairSrc $2 $4 }
     | '(' '+' Variadic')'                                     { AddSrc $3}
     | '(' '-' Variadic ')'                                    { SubSrc $3}
     | '(' '*' Variadic ')'                                    { MulSrc $3}
     | '(' '/' Variadic ')'                                    { DivSrc (reverse $3)}
     | '(' "&&" Variadic ')'                                   { AndSrc $3}
     | '(' "||" Variadic ')'                                   { OrSrc $3}
     | '(' "**" SASA SASA ')'                                  { PowSrc $3 $4}
     | '(' "sqrt" SASA ')'                                     { SqrtSrc $3 }
     | '(' '>' Variadic ')'                                    { GtSrc $3 }
     | '(' '<' Variadic ')'                                    { LtSrc $3 }
     | '(' ">=" Variadic ')'                                   { GeqSrc $3 }
     | '(' "<=" Variadic ')'                                   { LeqSrc $3 }
     | '(' '=' Variadic ')'                                    { EqSrc $3 }
     | '(' "!=" Variadic ')'                                   { NeqSrc $3 }
     | '(' "++" SASA ')'                                       { IncrSrc $3 }
     | '(' "--" SASA ')'                                       { DecrSrc $3 }
     | '(' "head" SASA ')'                                     { HeadSrc $3 }
     | '(' "tail" SASA ')'                                     { TailSrc $3 }
     | '(' "fst" SASA ')'                                      { FstSrc $3 }
     | '(' "snd" SASA ')'                                      { SndSrc $3 }
     | '(' "if" SASA SASA SASA ')'                             { IfSrc $3 $4 $5 }
     | '(' "not" SASA ')'                                      { NotSrc $3 }
     | '(' "let" '(' var SASA ')' SASA ')'                     { LetPSrc ([$4], [$5]) $7 }
     | '(' "let*" '(' var SASA ')' SASA ')'                    { LetSSrc ([$4], [$5]) $7 }
     | '(' "let" '(' Asig ')' SASA ')'                         { LetPSrc $4 $6 }
     | '(' "let*" '(' Asig ')' SASA ')'                        { LetSSrc $4 $6 }
     | '(' "letrec" '(' var SASA ')' SASA ')'                  { LetRSrc $4 $5 $7 }
     | '[' List ']'                                            { ListSrc $2}
     | '[' ']'                                                 { NillSrc }
     | '(' "cond" Prop ')'                                     { CondSrc (init $3) (fst (last $3)) }
     | '(' "lambda" '('Ids')' SASA ')'                         { LambdaSrc $4 $6 }
     | '(' '-' SASA ')'                                        { NegSrc $3 }
     | '(' ':' SASA SASA')'                                    { AppendSrc $3 $4}
     | '(' "concat" SASA SASA')'                               { ConcatSrc $3 $4}
     | '(' SASA Res ')'                                        { AppSrc $2 (reverse $3) }

Variadic : SASA Res                             { $1 : $2}

Res : SASA                                      { [$1] }
    | SASA Res                                  { $1 : $2 }

Asig : '(' var SASA ')'                         { ([$2],[$3]) }
     | '(' var SASA')' Asig                     { let (vs,ss) = $5 in ($2:vs, $3:ss) }

List : SASA                                     { [$1] }
     | SASA ','  List                           { $1 : $3 }

Prop : '[' SASA SASA ']' '[' "else" SASA ']'    { [($2, $3),($7,$7)] }
     | '[' SASA SASA ']' Prop                   { ($2,$3) : $5 }

Ids : var                                       { [$1] }
    | var Ids                                   { $1 : $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data SASA
  = NumSrc  Double
  | BooleanSrc Bool
  | PairSrc SASA SASA
  | VarSrc  String
  | AddSrc [SASA]
  | SubSrc [SASA]
  | MulSrc [SASA]
  | DivSrc [SASA]
  | AndSrc [SASA]
  | OrSrc [SASA]
  | PowSrc SASA SASA
  | SqrtSrc SASA
  | GtSrc [SASA]
  | LtSrc [SASA]
  | GeqSrc [SASA]
  | LeqSrc [SASA]
  | EqSrc [SASA]
  | NeqSrc [SASA]
  | IncrSrc SASA
  | DecrSrc SASA
  | HeadSrc SASA
  | TailSrc SASA
  | FstSrc SASA
  | SndSrc SASA
  | IfSrc SASA SASA SASA
  | NotSrc SASA
  | LetSSrc ([String], [SASA]) SASA
  | LetPSrc ([String], [SASA]) SASA
  | LetRSrc String SASA SASA
  | ListSrc [SASA]
  | NillSrc
  | CondSrc [(SASA, SASA)] SASA
  | LambdaSrc [String] SASA
  | AppSrc SASA [SASA]
  | NegSrc SASA
  | ConcatSrc SASA SASA
  | AppendSrc SASA SASA
  deriving (Show, Eq)

main = getContents >>= print . parse . lexer

}
