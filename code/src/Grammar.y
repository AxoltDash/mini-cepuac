{
{-|
Module      : Grammar
Description : Gramática de Mini-Cepuac (Happy) y definición del ASA.

Este archivo se procesa con Happy para generar @Grammar.hs@:

>   happy --ghc -o src/Grammar.hs src/Grammar.y

Contiene cuatro cosas:

1. La gramática de __programas__ ('Item'): una secuencia de definiciones
   (@define@, @type@) y expresiones.
2. La gramática de __expresiones__ ('ASA'): sintaxis prefija con paréntesis
   al estilo Lisp.
3. La gramática de __tipos__ ('Type'): tipos base, listas, registros,
   flechas y refinamientos @{ v : T | p }@.
4. La gramática de __predicados__ ('Pred'): lógica proposicional sobre
   comparaciones de la variable refinada con constantes.

Después del separador @%%@ viene la gramática y, al final, en el bloque
entre llaves, las definiciones de los tipos de datos que construye.

Parte del azúcar sintáctica se resuelve aquí mismo: las aplicaciones de
varios argumentos se traducen a aplicaciones anidadas de uno, y un @let@
con varias ligaduras @(let ((x 1) (y 2)) cuerpo)@ a @let@s anidados. Las
lambdas de varios parámetros se desazucaran en "Desugar", cuando ya se
conocen los alias de tipos.
-}
module Grammar where
import Lexer
import Data.List (intercalate)
}

%name parse ASA
%name parseProgram Program
%tokentype { Token }
%error { parseError }

%token
      var        { TokenId $$ }
      int        { TokenInt $$ }
      double     { TokenDouble $$ }
      str        { TokenStr $$ }
      "#t"       { TokenBool True }
      "#f"       { TokenBool False }
      ':'        { TokenColon }
      '('        { TokenLParen }
      ')'        { TokenRParen }
      '+'        { TokenPlus }
      '-'        { TokenMinus }
      '/'        { TokenSlash }
      '*'        { TokenMul }
      '%'        { TokenMod }
      "++"       { TokenConcat }
      "&&"       { TokenAnd }
      "||"       { TokenOr }
      "not"      { TokenNot }
      "let"      { TokenLet }
      "letrec"   { TokenLetrec }
      "define"   { TokenDefine }
      "type"     { TokenType }
      "if"       { TokenIf }
      "lambda"   { TokenLambda }
      "len"      { TokenLen }
      "toInt"    { TokenToInt }
      "toDouble" { TokenToDouble }
      "list"     { TokenList }
      "cons"     { TokenCons }
      "head"     { TokenHead }
      "tail"     { TokenTail }
      "nth"      { TokenNth }
      "set"      { TokenSet }
      "replicate" { TokenReplicate }
      "record"   { TokenRecord }
      "get"      { TokenGet }
      "with"     { TokenWith }
      "int"      { TokenTInt }
      "double"   { TokenTDouble }
      "string"   { TokenTString }
      "boolean"  { TokenTBoolean }
      "->"       { TokenArrow }
      '{'        { TokenLCurly }
      '|'        { TokenPipe }
      '}'        { TokenRCurly }
      "=="       { TokenEqEq }
      "!="       { TokenNeq }
      '<'        { TokenLT }
      "<="       { TokenLE }
      '>'        { TokenGT }
      ">="       { TokenGE }

-- Precedencias. La flecha asocia a la derecha (a -> b -> c = a -> (b -> c))
-- y "list" liga más fuerte que la flecha (list int -> int = (list int) -> int).
-- En los predicados, && liga más fuerte que || y "not" más fuerte que ambos.
%right "->"
%nonassoc "list"
%left "||"
%left "&&"
%nonassoc "not"
%%

-- Programas ----------------------------------------------------------------
--
-- Un programa (archivo o línea del REPL) es una secuencia de definiciones y
-- expresiones. Las definiciones son recursivas y persisten en el REPL.

Program : Items                      { $1 }

Items : {- vacío -}                  { [] }
      | Items Item                   { $1 ++ [$2] }

Item : '(' "define" var ':' Type ASA ')'                { DefVal $3 $5 $6 }
     | '(' "define" '(' var Vars ')' ':' Type ASA ')'   { DefFun $4 $8 $5 $9 }
     | '(' "type" var Type ')'                          { DefType $3 $4 }
     | ASA                                              { Expr $1 }

-- Expresiones ------------------------------------------------------------
--
-- Toda operación va entre paréntesis con el operador al frente.

ASA :  var                            { Id $1 }
     | int                            { AInt $1 }
     | double                         { ADouble $1 }
     | str                            { AStr $1 }
     | "#t"                           { ABool True }
     | "#f"                           { ABool False }
     -- aritmética (int con int o double con double)
     | '(' '+' ASA ASA ')'            { Add $3 $4 }
     | '(' '-' ASA ASA ')'            { Sub $3 $4 }
     | '(' '*' ASA ASA ')'            { Mul $3 $4 }
     | '(' '/' ASA ASA ')'            { Div $3 $4 }
     | '(' '%' ASA ASA ')'            { Mod $3 $4 }
     -- cadenas y listas
     | '(' "++" ASA ASA ')'           { Concat $3 $4 }
     | '(' "len" ASA ')'              { Len $3 }
     | '(' "list" Args0 ')'           { ListLit $3 }
     | '(' "cons" ASA ASA ')'         { Cons $3 $4 }
     | '(' "head" ASA ')'             { Head $3 }
     | '(' "tail" ASA ')'             { Tail $3 }
     | '(' "nth" ASA ASA ')'          { Nth $3 $4 }
     | '(' "set" ASA ASA ASA ')'      { SetNth $3 $4 $5 }
     | '(' "replicate" ASA ASA ')'    { Replicate $3 $4 }
     -- registros
     | '(' "record" Fields ')'        { Record $3 }
     | '(' "get" ASA var ')'          { Get $3 $4 }
     | '(' "with" ASA var ASA ')'     { With $3 $4 $5 }
     -- conversiones
     | '(' "toInt" ASA ')'            { ToInt $3 }
     | '(' "toDouble" ASA ')'         { ToDouble $3 }
     -- lógica y comparaciones
     | '(' "&&" ASA ASA ')'           { And $3 $4 }
     | '(' "||" ASA ASA ')'           { Or $3 $4 }
     | '(' "not" ASA ')'              { Not $3 }
     | '(' Cmp ASA ASA ')'            { Cmp $2 $3 $4 }
     -- control y ligado
     | '(' "if" ASA ASA ASA ')'       { If $3 $4 $5 }
     | '(' "lambda" ':' Type '(' Vars ')' ASA ')'      { LambdaN $4 $6 $8 }
     | '(' "let" '(' var ':' Type ASA ')' ASA ')'      { Let ($4, Just $6) $7 $9 }
     | '(' "let" '(' Bindings ')' ASA ')'  { foldr (\(x, t, v) b -> Let (x, t) v b) $6 $4 }
     | '(' "letrec" '(' var ':' Type ASA ')' ASA ')'   { Letrec ($4, $6) $7 $9 }
     | '(' ASA Args ')'               { foldl App $2 $3 }

Cmp : "=="  { Eq }
    | "!="  { Neq }
    | '<'   { Lt }
    | "<="  { Le }
    | '>'   { Gt }
    | ">="  { Ge }

-- uno o más argumentos / parámetros
Args : ASA                           { [$1] }
     | Args ASA                      { $1 ++ [$2] }

Args0 : {- vacío -}                  { [] }
      | Args0 ASA                    { $1 ++ [$2] }

Vars : var                           { [$1] }
     | Vars var                      { $1 ++ [$2] }

-- ligaduras de un let múltiple (let ((x 1) (y : int 2)) cuerpo): con
-- anotación de tipo o sin ella, se evalúan en orden (cada una ve a las
-- anteriores)
Bindings : Binding                   { [$1] }
         | Bindings Binding          { $1 ++ [$2] }

Binding : '(' var ':' Type ASA ')'   { ($2, Just $4, $5) }
        | '(' var ASA ')'            { ($2, Nothing, $3) }

Fields : Field                       { [$1] }
       | Fields Field                { $1 ++ [$2] }

Field : '(' var ASA ')'              { ($2, $3) }

-- Tipos -------------------------------------------------------------------
--
--   Type ::= ( Type ) | int | double | string | boolean | alias
--          | list Type | ( record ( campo Type ) ... )
--          | Type -> Type | { var : Type | Pred }

Type : '(' Type ')'                  { $2 }
     | BaseType                      { TBase $1 }
     | var                           { TName $1 }
     | "list" Type                   { TList $2 }
     | '(' "record" TFields ')'      { TRecord $3 }
     | Arrow                         { $1 }
     | RefinementType                { $1 }

BaseType : "int"      { TInt }
         | "double"   { TDouble }
         | "string"   { TString }
         | "boolean"  { TBool }

TFields : TField                     { [$1] }
        | TFields TField             { $1 ++ [$2] }

TField : '(' var Type ')'            { ($2, $3) }

Arrow : Type "->" Type   { Arrow $1 $3 }

-- { v : T | p }  donde p habla de v. T puede ser a su vez un refinamiento;
-- el verificador intersecta ambos predicados.
RefinementType : '{' var ':' Type '|' Pred '}'   { Refinement $2 $4 $6 }

-- Predicados de refinamiento -----------------------------------------------
--
--   Pred ::= Pred || Pred | Pred && Pred | not Pred | ( Pred )
--          | Term op Lit | Lit op Term
--   Term ::= var | len var
--   Lit  ::= int | double | -int | -double | str | #t | #f

Pred : Pred "||" Pred    { POr $1 $3 }
     | Pred "&&" Pred    { PAnd $1 $3 }
     | "not" Pred        { PNot $2 }
     | '(' Pred ')'      { $2 }
     | Term Cmp Lit      { PCmp $1 $2 $3 }
     | Lit Cmp Term      { PCmp $3 (flipCmp $2) $1 }

Term : var               { TVar $1 }
     | "len" var         { TLen $2 }

Lit : int                { LInt $1 }
    | double             { LDouble $1 }
    | '-' int            { LInt (negate $2) }
    | '-' double         { LDouble (negate $2) }
    | str                { LStr $1 }
    | "#t"               { LBool True }
    | "#f"               { LBool False }

{

-- | Error sintáctico: muestra los siguientes tokens para ubicar el problema.
parseError :: [Token] -> a
parseError [] = error "Parse error: unexpected end of input"
parseError ts = error ("Parse error near: " ++ unwords (map showTok (take 6 ts)))
  where showTok (TokenId s) = s
        showTok (TokenInt n) = show n
        showTok (TokenDouble d) = show d
        showTok (TokenStr s) = show s
        showTok t = show t

-- | Elementos de un programa.
data Item
  = DefVal String Type ASA            -- ^ @(define x : T e)@
  | DefFun String Type [String] ASA   -- ^ @(define (f x y) : T e)@, T es el tipo función completo
  | DefType String Type               -- ^ @(type nombre T)@: alias de tipo
  | Expr ASA                          -- ^ una expresión a evaluar
  deriving (Show, Eq)

-- | Tipos base del lenguaje.
data Base = TInt | TDouble | TString | TBool
  deriving (Eq, Ord)

instance Show Base where
  show TInt    = "int"
  show TDouble = "double"
  show TString = "string"
  show TBool   = "boolean"

-- | Tipos tal como los escribe el programador. El verificador ("Checker")
-- los traduce a su forma normal ('Checker.RType') después de que
-- "Desugar" sustituye los alias ('TName').
data Type
  = TBase Base                    -- ^ @int@, @double@, @string@, @boolean@
  | TName String                  -- ^ alias declarado con @(type nombre T)@
  | TList Type                    -- ^ @list T@
  | TRecord [(String, Type)]      -- ^ @(record (campo T) ...)@
  | Arrow Type Type               -- ^ @T1 -> T2@
  | Refinement String Type Pred   -- ^ @{ v : T | p }@: variable, tipo y predicado
  deriving (Eq)

instance Show Type where
  show (TBase b) = show b
  show (TName n) = n
  show (TList t) = "list " ++ showAtom t
    where showAtom a@(Arrow _ _) = "(" ++ show a ++ ")"
          showAtom a = show a
  show (TRecord fs) = "(record " ++ unwords ["(" ++ f ++ " " ++ show t ++ ")" | (f, t) <- fs] ++ ")"
  show (Arrow d c) = showDom d ++ " -> " ++ show c
    where showDom t@(Arrow _ _) = "(" ++ show t ++ ")"
          showDom t = show t
  show (Refinement v t p) = "{ " ++ v ++ " : " ++ show t ++ " | " ++ show p ++ " }"

-- | Operadores de comparación, compartidos por expresiones y predicados.
data CmpOp = Eq | Neq | Lt | Le | Gt | Ge
  deriving (Eq, Ord)

instance Show CmpOp where
  show Eq = "=="
  show Neq = "!="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

-- | Operador equivalente al intercambiar los operandos: @(a < b) == (b > a)@.
flipCmp :: CmpOp -> CmpOp
flipCmp Lt = Gt
flipCmp Le = Ge
flipCmp Gt = Lt
flipCmp Ge = Le
flipCmp op = op

-- | Negación lógica de una comparación: @not (a < b) == (a >= b)@.
negCmp :: CmpOp -> CmpOp
negCmp Eq = Neq
negCmp Neq = Eq
negCmp Lt = Ge
negCmp Le = Gt
negCmp Gt = Le
negCmp Ge = Lt

-- | Literales que aparecen dentro de los predicados.
data Lit = LInt Integer | LDouble Double | LStr String | LBool Bool
  deriving (Eq)

instance Show Lit where
  show (LInt n) = show n
  show (LDouble d) = show d
  show (LStr s) = show s
  show (LBool True) = "#t"
  show (LBool False) = "#f"

-- | Lado izquierdo de una comparación en un predicado: la variable refinada
-- o su longitud (para string y list).
data Term = TVar String | TLen String
  deriving (Eq)

instance Show Term where
  show (TVar v) = v
  show (TLen v) = "len " ++ v

-- | Lenguaje de predicados: lógica proposicional sobre comparaciones de la
-- variable refinada con constantes. 'PTrue' y 'PFalse' no tienen sintaxis
-- concreta; los produce el verificador al imprimir tipos.
data Pred
  = PTrue
  | PFalse
  | PCmp Term CmpOp Lit
  | PAnd Pred Pred
  | POr Pred Pred
  | PNot Pred
  deriving (Eq)

instance Show Pred where
  show PTrue = "true"
  show PFalse = "false"
  show (PCmp t op l) = show t ++ " " ++ show op ++ " " ++ show l
  show (PAnd a b) = showConj a ++ " && " ++ showConj b
    where showConj p@(POr _ _) = "(" ++ show p ++ ")"
          showConj p = show p
  show (POr a b) = show a ++ " || " ++ show b
  show (PNot p@(PCmp _ _ _)) = "not " ++ show p
  show (PNot p) = "not (" ++ show p ++ ")"

-- | Árbol de sintaxis abstracta de las expresiones.
data ASA
  = AInt Integer                -- ^ literal int
  | ADouble Double              -- ^ literal double
  | AStr String                 -- ^ literal string
  | ABool Bool                  -- ^ @#t@ / @#f@
  | Id String                   -- ^ variable
  | Add ASA ASA                 -- ^ @(+ a b)@
  | Sub ASA ASA                 -- ^ @(- a b)@
  | Mul ASA ASA                 -- ^ @(* a b)@
  | Div ASA ASA                 -- ^ @(/ a b)@: entera en int, real en double
  | Mod ASA ASA                 -- ^ @(% a b)@: residuo entero
  | Concat ASA ASA              -- ^ @(++ s t)@: cadenas o listas
  | Len ASA                     -- ^ @(len s)@: cadenas o listas
  | ListLit [ASA]               -- ^ @(list e1 ... en)@
  | Cons ASA ASA                -- ^ @(cons x xs)@
  | Head ASA                    -- ^ @(head xs)@, exige len xs >= 1
  | Tail ASA                    -- ^ @(tail xs)@, exige len xs >= 1
  | Nth ASA ASA                 -- ^ @(nth xs i)@, exige 0 <= i < len xs
  | SetNth ASA ASA ASA          -- ^ @(set xs i v)@: copia con la posición i cambiada
  | Replicate ASA ASA           -- ^ @(replicate n v)@: lista de n copias de v
  | Record [(String, ASA)]      -- ^ @(record (f e) ...)@
  | Get ASA String              -- ^ @(get r f)@
  | With ASA String ASA         -- ^ @(with r f e)@: copia con el campo f cambiado
  | ToInt ASA                   -- ^ @(toInt d)@: trunca hacia cero
  | ToDouble ASA                -- ^ @(toDouble n)@
  | And ASA ASA                 -- ^ @(&& a b)@
  | Or ASA ASA                  -- ^ @(|| a b)@
  | Not ASA                     -- ^ @(not a)@
  | Cmp CmpOp ASA ASA           -- ^ @(== a b)@, @(< a b)@, ...
  | If ASA ASA ASA              -- ^ @(if c t e)@
  | LambdaN Type [String] ASA   -- ^ @(lambda : T (x y) b)@ antes de desazucarar
  | Lambda Type String ASA      -- ^ lambda de un parámetro con su tipo función
  | App ASA ASA                 -- ^ @(f a)@
  | Let (String, Maybe Type) ASA ASA  -- ^ @(let (x : T v) b)@ o @(let ((x v) ...) b)@
  | Letrec (String, Type) ASA ASA     -- ^ @(letrec (x : T v) b)@: x visible en v
  deriving (Show, Eq)

}
