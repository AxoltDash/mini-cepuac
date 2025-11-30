module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

tc :: ASA -> Type
tc (Num n) = Number
tc (Boolean b) = Bool 
tc (Add i d) = case (tc i, tc d) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (Sub i d) = case (tc i, tc d) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (Mul i d) = case (tc i, tc d) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (Div i d) = case (tc i, tc d) of
  (Number, Number) -> Number
  _ -> error "Type mismatch"
tc (And i d) = case (tc i, tc d) of
  (Bool, Bool) -> Bool
  _ -> error "Type mismatch"
tc (Or i d) = case (tc i, tc d) of
  (Bool, Bool) -> Bool
  _ -> error "Type mismatch"
tc (Not b) = case tc b of
  _ -> Bool
