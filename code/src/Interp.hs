{-# LANGUAGE BangPatterns #-}
module Interp where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

-- Indica si una expresion se puede reducir y guarda el ambiente de definicion para las funciones
data Tag
  = Value
  | Closure Env
  | ASAT
  deriving (Show, Eq)

data Value
  = NumV Double 
  | BooleanV Bool

-- Configuraciones del sistema de transiciones
type Config = (ASA, Env, Tag) 

-- Ambiente sobre el cual se evalua una expresion
type Env = [(String, ASA, Tag)]

instance Show Value where
  show (NumV n) = show n
  show (BooleanV True) = "#t"
  show (BooleanV False) = "#f"

toValue :: ASA -> Value
toValue (Num n) = NumV n
toValue (Boolean b) = BooleanV b 

step :: Config -> Config
step (Num n, env, ASAT) = (Num n, env, Value) 
step (Boolean b, env, ASAT) = (Boolean b, env, Value) 
step (Id x, env, ASAT) = case lookup x env of
                      Just (v, tag)  -> (v, env, tag)
                      Nothing -> error ("Variable " ++ x ++ " not found")

step (Add (Num a) (Num b), env, ASAT) = (Num (a + b), env, Value)
step (Add (Num a) b, env, ASAT) = (Add (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Add a b, env, ASAT) = (Add (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (And (Boolean a) (Boolean b), env, ASAT) = (Boolean (a && b), env, Value)
step (And (Boolean a) b, env, ASAT) = (And (Boolean a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (And a b, env, ASAT) = (And (getASA $ step (a, env, ASAT)) b , env, ASAT)

step (Or (Boolean a) (Boolean b), env, ASAT) = (Boolean (a || b), env, Value)
step (Or (Boolean a) b, env, ASAT) = (Or (Boolean a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Or a b, env, ASAT) = (Or (getASA $ step (a, env, ASAT)) b , env, ASAT)

step (Mul (Num a) (Num b), env, ASAT) = (Num (a * b), env, Value)
step (Mul (Num a) b, env, ASAT) = (Mul (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Mul a b, env, ASAT) = (Mul (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (Div (Num a) (Num 0), env, ASAT) = error "Division by zero"
step (Div (Num a) (Num b), env, ASAT) = (Num (a / b), env, Value)
step (Div (Num a) b, env, ASAT) = (Div (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Div a b, env, ASAT) = (Div (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (Not (Boolean b), env, ASAT) = (Boolean (not b), env, Value)
step (Not b, env, ASAT) = (Not (getASA $ step (b, env, ASAT)), env, ASAT)

-- Recupera la expresion (ASA) de una configuracion
getASA :: Config -> ASA
getASA (e, _, _) = e

-- Resuelve variables a expresiones
lookup :: String -> Env -> Maybe (ASA, Tag)
lookup _ [] = Nothing
lookup x ((y, v, t):env)
  | x == y    = Just (v, t)
  | otherwise = lookup x env

-- Aisla la evalucion de una expresion
strict :: Config -> Config 
strict (e, env, ASAT) = let (e', env', tag') = step (e, env, ASAT) in strict (e', env', tag')
strict (e, env, tag) = (e, env, tag)

interpaux :: Config -> ASA
interpaux (e, env, ASAT) = let (e', env', tag') = step (e, env, ASAT) in interpaux (e', env', tag')
interpaux (e, env, tag) = e

interp :: ASA -> Value
interp e = toValue $ interpaux (e, [], ASAT)
