module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- COnfiguraciones del sistema de transicion. (revisar)

tc :: ConfigT -> Type
tc (_, (Num n)) = Number
tc (_, (Boolean b)) = Bool
tc (g, Id s) = lookup g s
tc (g, (Add i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (g, (Sub i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (g, (Mul i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (g, (Div i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (g, (And i d)) = case (tc (g, i), tc (g, d)) of
  (Bool, Bool) -> Bool
  _ -> error "Type mismatch"
tc (g, (Or i d)) = case (tc (g, i), tc (g, d)) of
  (Bool, Bool) -> Bool
  _ -> error "Type mismatch"
tc (g, (Not b)) = case tc (g, b) of
  _ -> Bool
tc (g, (Let (i, t) a c)) = tc ((i, t):g, c)

lookup :: Gamma -> String -> Type
lookup [] s = error "Free variable"
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s
