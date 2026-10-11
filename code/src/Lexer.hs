{-|
Module      : Lexer
Description : Analizador léxico de Mini-Cepuac.

Convierte el texto fuente en una lista de 'Token's que consume el parser
generado por Happy ("Grammar"). Reglas principales:

* Los espacios se ignoran y @;@ inicia un comentario hasta el fin de línea.
* Los operadores de dos caracteres (@&&@, @||@, @==@, @!=@, @<=@, @>=@,
  @->@, @++@) se reconocen antes que los de uno para que @<=@ no se lea
  como @<@ seguido de @=@.
* Un número es @int@ salvo que lleve punto decimal seguido de dígitos
  (@3@ es int, @3.0@ es double). Un @-@ pegado a un dígito forma un literal
  negativo (@-5@); con espacio es el operador resta (@(- 5 3)@).
* Las cadenas van entre comillas dobles y admiten los escapes
  @\\"  \\\\  \\n  \\t@.
* @#t@ y @#f@ son los booleanos.
* Un identificador empieza con letra ASCII y sigue con letras, dígitos,
  @_@, @-@, @?@ o @'@ (así @red?@ y @make-set@ son identificadores; el operador resta
  siempre va separado por espacios). Las palabras reservadas se distinguen
  en 'lexKeywordOrId'.
-}
module Lexer (lexer, Token(..)) where
import Data.Char (isDigit, isSpace)

-- | Tokens del lenguaje.
data Token
  = TokenInt Integer     -- ^ literal entero: @3@, @42@, @-7@
  | TokenDouble Double   -- ^ literal de punto flotante: @3.0@, @2.5@
  | TokenStr String      -- ^ literal de cadena: @"hola"@
  | TokenBool Bool       -- ^ @#t@, @#f@
  | TokenId String       -- ^ identificador
  | TokenLParen          -- ^ @(@
  | TokenRParen          -- ^ @)@
  | TokenPlus            -- ^ @+@
  | TokenMinus           -- ^ @-@
  | TokenSlash           -- ^ @/@
  | TokenMul             -- ^ @*@
  | TokenMod             -- ^ @%@
  | TokenConcat          -- ^ @++@
  | TokenAnd             -- ^ @&&@
  | TokenOr              -- ^ @||@
  | TokenNot             -- ^ @not@
  | TokenLet             -- ^ @let@
  | TokenIf              -- ^ @if@
  | TokenLetrec          -- ^ @letrec@
  | TokenDefine          -- ^ @define@
  | TokenType            -- ^ @type@
  | TokenLambda          -- ^ @lambda@
  | TokenList            -- ^ @list@ (expresión y tipo)
  | TokenCons            -- ^ @cons@
  | TokenHead            -- ^ @head@
  | TokenTail            -- ^ @tail@
  | TokenNth             -- ^ @nth@
  | TokenSet             -- ^ @set@
  | TokenReplicate       -- ^ @replicate@
  | TokenRecord          -- ^ @record@ (expresión y tipo)
  | TokenGet             -- ^ @get@
  | TokenWith            -- ^ @with@
  | TokenLen             -- ^ @len@
  | TokenToInt           -- ^ @toInt@
  | TokenToDouble        -- ^ @toDouble@
  | TokenTInt            -- ^ tipo @int@
  | TokenTDouble         -- ^ tipo @double@
  | TokenTString         -- ^ tipo @string@
  | TokenTBoolean        -- ^ tipo @boolean@
  | TokenColon           -- ^ @:@
  | TokenArrow           -- ^ @->@
  | TokenLCurly          -- ^ @{@
  | TokenPipe            -- ^ @|@
  | TokenRCurly          -- ^ @}@
  | TokenEqEq            -- ^ @==@
  | TokenNeq             -- ^ @!=@
  | TokenLT              -- ^ @<@
  | TokenLE              -- ^ @<=@
  | TokenGT              -- ^ @>@
  | TokenGE              -- ^ @>=@
  deriving (Show, Eq)

-- | Tokeniza el programa completo. Aborta con 'error' ante un carácter que
-- no pertenece al lenguaje.
lexer :: String -> [Token]
lexer [] = []

lexer input@(c:cs)
  | isSpace c = lexer cs
  | c == ';'  = lexer (drop 1 $ dropWhile (/= '\n') input)
  -- operadores de dos caracteres, antes que los de uno
  | take 2 input == "&&" = TokenAnd    : lexer (drop 2 input)
  | take 2 input == "||" = TokenOr     : lexer (drop 2 input)
  | take 2 input == "==" = TokenEqEq   : lexer (drop 2 input)
  | take 2 input == "!=" = TokenNeq    : lexer (drop 2 input)
  | take 2 input == "<=" = TokenLE     : lexer (drop 2 input)
  | take 2 input == ">=" = TokenGE     : lexer (drop 2 input)
  | take 2 input == "->" = TokenArrow  : lexer (drop 2 input)
  | take 2 input == "++" = TokenConcat : lexer (drop 2 input)
  -- operadores de un carácter
  | c == '(' = TokenLParen   : lexer cs
  | c == ':' = TokenColon    : lexer cs
  | c == ')' = TokenRParen   : lexer cs
  | c == '+' = TokenPlus     : lexer cs
  -- '-' pegado a un dígito es un literal negativo: -5, -2.5
  | c == '-', (d:_) <- cs, isDigit d = negateTok (lexNumber cs)
  | c == '-' = TokenMinus    : lexer cs
  | c == '*' = TokenMul      : lexer cs
  | c == '/' = TokenSlash    : lexer cs
  | c == '%' = TokenMod      : lexer cs
  | c == '{' = TokenLCurly   : lexer cs
  | c == '}' = TokenRCurly   : lexer cs
  | c == '|' = TokenPipe     : lexer cs
  | c == '<' = TokenLT       : lexer cs
  | c == '>' = TokenGT       : lexer cs
  -- literales
  | c == '"' = lexString cs
  | c == '#' = case cs of
                 ('t':rest) -> TokenBool True : lexer rest
                 ('f':rest) -> TokenBool False : lexer rest
                 _ -> error "Error léxico: '#' debe ir seguido de 't' o 'f'"
  | isDigit c = lexNumber input
  | isAsciiLetter c = let (lexeme, rest) = span isIdChar input
                      in lexKeywordOrId lexeme : lexer rest
  | otherwise = error ("Error léxico: Caracter no reconocido '" ++ [c] ++ "'")

-- | Lee un literal numérico. Es entero salvo que contenga un punto decimal
-- seguido de al menos un dígito: @3@ -> int, @3.0@ -> double.
lexNumber :: String -> [Token]
lexNumber input =
  let (intPart, rest) = span isDigit input
  in case rest of
       ('.':r@(d:_)) | isDigit d ->
         let (fracPart, rest') = span isDigit r
         in TokenDouble (read (intPart ++ "." ++ fracPart)) : lexer rest'
       _ -> TokenInt (read intPart) : lexer rest

-- | Niega el primer token numérico de la lista (para literales negativos).
negateTok :: [Token] -> [Token]
negateTok (TokenInt n : rest) = TokenInt (negate n) : rest
negateTok (TokenDouble d : rest) = TokenDouble (negate d) : rest
negateTok ts = ts

-- | Lee el contenido de una cadena (ya se consumió la comilla inicial)
-- resolviendo los escapes @\\"  \\\\  \\n  \\t@.
lexString :: String -> [Token]
lexString = go ""
  where
    go _ [] = error "Error léxico: cadena sin cerrar"
    go acc ('"':rest) = TokenStr (reverse acc) : lexer rest
    go acc ('\\':e:rest) = case e of
      'n'  -> go ('\n':acc) rest
      't'  -> go ('\t':acc) rest
      '"'  -> go ('"':acc) rest
      '\\' -> go ('\\':acc) rest
      _    -> error ("Error léxico: escape desconocido '\\" ++ [e] ++ "'")
    go acc (x:rest) = go (x:acc) rest

-- | Distingue las palabras reservadas de los identificadores. El antiguo
-- tipo @number@ ya no existe; se avisa para facilitar la migración.
lexKeywordOrId :: String -> Token
lexKeywordOrId lexeme = case lexeme of
  "let"      -> TokenLet
  "if"       -> TokenIf
  "not"      -> TokenNot
  "letrec"   -> TokenLetrec
  "define"   -> TokenDefine
  "type"     -> TokenType
  "lambda"   -> TokenLambda
  "list"     -> TokenList
  "cons"     -> TokenCons
  "head"     -> TokenHead
  "tail"     -> TokenTail
  "nth"      -> TokenNth
  "set"      -> TokenSet
  "replicate" -> TokenReplicate
  "record"   -> TokenRecord
  "get"      -> TokenGet
  "with"     -> TokenWith
  "len"      -> TokenLen
  "toInt"    -> TokenToInt
  "toDouble" -> TokenToDouble
  "int"      -> TokenTInt
  "double"   -> TokenTDouble
  "string"   -> TokenTString
  "boolean"  -> TokenTBoolean
  "number"   -> error "Error léxico: el tipo 'number' fue reemplazado por 'int' y 'double'"
  _          -> TokenId lexeme

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

-- | Caracteres permitidos dentro de un identificador (después del primero).
isIdChar :: Char -> Bool
isIdChar c = isAsciiLetter c || isDigit c || c == '_' || c == '-' || c == '?' || c == '\''
