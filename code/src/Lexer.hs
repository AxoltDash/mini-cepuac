module Lexer (lexer, Token(..)) where
import Data.Char (isDigit, isSpace)

-- Definimos los tokens.
data Token
  = TokenNum Double
  | TokenBool Bool
  | TokenId String
  | TokenLParen
  | TokenRParen
  | TokenPlus
  | TokenMinus
  | TokenSlash
  | TokenMul
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenLet
  | TokenNumber
  | TokenBoolean
  | TokenColon
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []

lexer input@(c:cs)
  | isSpace c = lexer cs
  | take 2 input == "&&" = TokenAnd : lexer (drop 2 input)
  | take 2 input == "||" = TokenOr : lexer (drop 2 input)
  | c == '(' = TokenLParen   : lexer cs
  | c == ':' = TokenColon    : lexer cs 
  | c == ')' = TokenRParen   : lexer cs
  | c == '+' = TokenPlus     : lexer cs
  | c == '-' = TokenMinus    : lexer cs
  | c == '*' = TokenMul      : lexer cs
  | c == '/' = TokenSlash    : lexer cs
  | c == '#' = case cs of
                 ('t':rest) -> TokenBool True : lexer rest
                 ('f':rest) -> TokenBool False : lexer rest
                 _ -> error "Error léxico: '#' debe ir seguido de 't' o 'f'"
  | isDigit c = lexNumber input
  | isAsciiLetter c = let (lexeme, rest) = span (\x -> isAsciiLetter x || isDigit x) input
                      in lexKeywordOrId lexeme : lexer rest
  | otherwise = error ("Error léxico: Caracter no reconocido '" ++ [c] ++ "'")

lexNumber :: String -> [Token]
lexNumber input =
    let (numPart, rest) = span (\c -> isDigit c || c == '.') input
    in TokenNum (read numPart) : lexer rest

lexKeywordOrId :: String -> Token
lexKeywordOrId lexeme = case lexeme of
  "let" -> TokenLet
  "not" -> TokenNot
  "number" -> TokenNumber
  "boolean" -> TokenBoolean
  _      -> TokenId lexeme

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
