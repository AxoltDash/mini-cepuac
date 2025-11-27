module Lexer (lexer, Token(..)) where
import Data.Char (isDigit, isSpace)

-- Definimos los tokens.
data Token
  = TokenNum Double
  | TokenBool Bool
  | TokenVar String
-- Símbolos
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenComma
-- Operadores Aritmeticos
  | TokenPlus
  | TokenMinus
  | TokenSlash
  | TokenMul
  | TokenPow
  | TokenInc
  | TokenDec
-- Operadores Logicos
  | TokenAnd
  | TokenOr
-- Operadores de Comparacion
  | TokenLT
  | TokenGT
  | TokenLTE
  | TokenGTE
  | TokenNEQ
  | TokenEQ
-- Palabras reservadas
  | TokenLetp
  | TokenLets
  | TokenLetr
  | TokenIf
  | TokenNot
  | TokenLambda
  | TokenFst
  | TokenSnd
  | TokenHead
  | TokenTail
  | TokenElse
  | TokenSqrt
  | TokenCond
  | TokenAppend
  | TokenConcat
  deriving (Show, Eq)


lexer :: String -> [Token]
lexer [] = [] 


lexer input@(c:cs) -- Usamos `input@` para tener acceso a toda la cadena
-- espacios en blanco
  | isSpace c = lexer cs
-- Símbolos y operadores compuestos 
  | take 4 input == "let*" = TokenLets : lexer (drop 4 input)
  | take 2 input == "++" = TokenInc : lexer (drop 2 input)
  | take 2 input == "--" = TokenDec : lexer (drop 2 input)
  | take 2 input == "**" = TokenPow : lexer (drop 2 input)
  | take 2 input == "<=" = TokenLTE : lexer (drop 2 input)
  | take 2 input == ">=" = TokenGTE : lexer (drop 2 input)
  | take 2 input == "!=" = TokenNEQ : lexer (drop 2 input)
  | take 2 input == "&&" = TokenAnd : lexer (drop 2 input)
  | take 2 input == "||" = TokenOr : lexer (drop 2 input)
-- Símbolos y operadores simples
  | c == '=' = TokenEQ       : lexer cs  
  | c == '(' = TokenLParen   : lexer cs
  | c == ')' = TokenRParen   : lexer cs
  | c == '[' = TokenLBracket : lexer cs
  | c == ']' = TokenRBracket : lexer cs
  | c == ',' = TokenComma    : lexer cs
  | c == '+' = TokenPlus     : lexer cs
  | c == '*' = TokenMul      : lexer cs
  | c == '/' = TokenSlash    : lexer cs
  | c == '<' = TokenLT       : lexer cs
  | c == '>' = TokenGT       : lexer cs
  | c == ':' = TokenAppend   : lexer cs 
  | c == '-' = handleMinusOrNumber input -- Un `-` puede ser un operador o parte de un número
-- Booleanos
  | c == '#' = case cs of
                 ('t':rest) -> TokenBool True : lexer rest
                 ('f':rest) -> TokenBool False : lexer rest
                 _ -> error "Error léxico: '#' debe ir seguido de 't' o 'f'"
-- Números 
  | isDigit c = lexNumber input
-- keywords y variables
  | isAsciiLetter c = let (lexeme, rest) = span (\x -> isAsciiLetter x || isDigit x) input
                in lexKeywordOrVar lexeme : lexer rest
-- si no es nada de lo anterior mandamos error
  | otherwise = error ("Error léxico: Caracter no reconocido '" ++ [c] ++ "'")

-- Decide si un '-' es un operador o el inicio de un número negativo
handleMinusOrNumber :: String -> [Token]
handleMinusOrNumber ('-':rest@(c:_))
  | isDigit c = lexNumber ('-':rest)  -- Es un número negativo
  | otherwise = TokenMinus : lexer rest -- Es un operador de resta

-- Extrae un número
lexNumber :: String -> [Token]
lexNumber input =
    let (numPart, rest) = case input of
                            ('-':cs) -> let (digits, remaining) = span (\c -> isDigit c || c == '.') cs
                                        in ('-':digits, remaining)
                            _ -> span (\c -> isDigit c || c == '.') input
    in TokenNum (read numPart) : lexer rest

-- Convierte un lexema en una keyword o variable
lexKeywordOrVar :: String -> Token
lexKeywordOrVar lexeme = case lexeme of
  "let"    -> TokenLetp
  "letrec" -> TokenLetr
  "if"     -> TokenIf
  "lambda" -> TokenLambda
  "fst"    -> TokenFst
  "snd"    -> TokenSnd
  "head"   -> TokenHead
  "tail"   -> TokenTail
  "else"   -> TokenElse
  "sqrt"   -> TokenSqrt
  "cond"   -> TokenCond
  "not"    -> TokenNot
  "concat" -> TokenConcat
  _        -> TokenVar lexeme -- Si no es una keyword, es una variable

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
