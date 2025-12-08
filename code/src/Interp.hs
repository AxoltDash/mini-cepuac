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
type Config = (ASA, Type, Env) 

-- Ambiente sobre el cual se evalua una expresion
type Env = [(String, Value)]

instance Show Value where
  show (NumV n) = show n
  show (BoolV True) = "#t"
  show (BoolV False) = "#f"

step :: Config -> Value
step (ANum n, _,env) = NumV n
step (ABool b, _,env) = BoolV b
step (Id x, _,env) = case lookup x env of
                      Just v  -> v
                      Nothing -> error ("Variable " ++ x ++ " not found")
step (Add i d, r,env) = NumV ((unboxNum $ step(i, r,env)) + (unboxNum $ step(d, r,env)))
step (Sub i d, r,env) = NumV ((unboxNum $ step(i, r,env)) - (unboxNum $ step(d, r,env)))
step (Mul i d, r,env) = NumV ((unboxNum $ step(i, r,env)) * (unboxNum $ step(d, r,env)))
step (Div i d, Refinement Number Zero, env) = error $ "This expression contain a division by zero. "
step (Div i d, Refinement Number MaybeZero, env) = error $"This expression may constain a division by zero. "
step (Div i d, r,env) = NumV ((unboxNum $ step(i, r,env)) / (unboxNum $ step(d, r,env)))
step (Not b, r,env) = BoolV (not (unboxBool $ step(b, r,env)))
step (And i d, r,env) = BoolV ((unboxBool $ step(i, r,env)) && (unboxBool $ step(d, r,env)))
step (Or i d, r,env) = BoolV ((unboxBool $ step(i, r,env)) && (unboxBool $ step(d, r,env)))
step (Let (i,_) a b, r,env) = let a' = step (a, r,env)
  in step (b, r,(i, a'):env)
step (Lambda _ x b, r,env) = Closure x b env
step (App f a, r,env) = case step (f, r,env) of
    Closure x b env' -> let a' = step (a, r,env) in step (b, r,(x, a'):env')

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

interp :: ASA -> Type -> Value
interp e t = step (e, t, [])
