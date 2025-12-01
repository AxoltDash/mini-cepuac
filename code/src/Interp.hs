{-# LANGUAGE BangPatterns #-}
module Interp where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Checker hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

data Value
  = NumV Double 
  | BoolV Bool
  | Closure String ASA Env

-- Configuraciones del sistema de transiciones
type Config = (ASA, Env) 

-- Ambiente sobre el cual se evalua una expresion
type Env = [(String, Value)]

instance Show Value where
  show (NumV n) = show n
  show (BoolV True) = "#t"
  show (BoolV False) = "#f"

step :: Config -> Value
step (ANum n, env) = NumV n 
step (ABool b, env) = BoolV b
step (Id x, env) = case lookup x env of
                      Just v  -> v
                      Nothing -> error ("Variable " ++ x ++ " not found")
step (Add i d, env) = NumV ((unboxNum $ step(i, env)) + (unboxNum $ step(d, env)))
step (Sub i d, env) = NumV ((unboxNum $ step(i, env)) - (unboxNum $ step(d, env)))
step (Mul i d, env) = NumV ((unboxNum $ step(i, env)) * (unboxNum $ step(d, env)))
step (Div i d, env) = NumV ((unboxNum $ step(i, env)) / (unboxNum $ step(d, env)))
step (Not b, env) = BoolV (not (unboxBool $ step(b, env)))
step (And i d, env) = BoolV ((unboxBool $ step(i, env)) && (unboxBool $ step(d, env)))
step (Or i d, env) = BoolV ((unboxBool $ step(i, env)) && (unboxBool $ step(d, env)))
step (Let (i,_) a b, env) = let a' = step (a, env)
  in step (b, (i, a'):env)
step (Lambda _ x b, env) = Closure x b env
step (App f a, env) = case step (f, env) of
    Closure x b env' -> let a' = step (a, env) in step (b, (x, a'):env')

unboxNum :: Value -> Double
unboxNum (NumV n) = n

unboxBool :: Value -> Bool
unboxBool (BoolV b) = b

-- Resuelve variables a expresiones
lookup :: String -> Env -> Maybe Value
lookup _ [] = Nothing
lookup x ((y, v):env)
  | x == y    = Just v
  | otherwise = lookup x env

interp :: ASA -> Value
interp e = step (e, [])
