module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- COnfiguraciones del sistema de transicion. (revisar)

tc :: ConfigT -> RefinementType
tc (_, (Num n)) 
              | n == 0 = Refinement "v" Number Zero
              | otherwise = Refinement "v" Number NonZero
tc (_, (ABool b)) = Refinement "v" Boolean NonZero
tc (g, Id s) = lookup g s
tc (g, (Add i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Number _, Refinement _ Number _) -> Refinement "v" Number MaybeZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator + : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Sub i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Number _, Refinement _ Number _) -> Refinement "v" Number MaybeZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator - : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Mul i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Number NonZero, Refinement _ Number NonZero) -> Refinement "v" Number NonZero
  (Refinement _ Number Zero, Refinement _ Number _) -> Refinement "v" Number Zero
  (Refinement _ Number _, Refinement _ Number Zero) -> Refinement "v" Number Zero
  (Refinement _ Number _, Refinement _ Number _) -> Refinement "v" Number MaybeZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator * : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Div i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Number NonZero, Refinement _ Number _) -> Refinement "v" Number NonZero
  (Refinement _ Number Zero, Refinement _ Number _) -> Refinement "v" Number Zero
  (Refinement _ Number _, Refinement _ Number _) -> Refinement "v" Number MaybeZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator / : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (And i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Boolean _, Refinement _ Boolean _) -> Refinement "v" Boolean NonZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator && : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Or i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement _ Boolean _, Refinement _ Boolean _) -> Refinement "v" Boolean NonZero
  (Refinement _ t1 _, Refinement _ t2 _) -> error $ "Bad operand types for operator || : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Not b)) = case tc (g, b) of
  Refinement _ Boolean _ -> Refinement "v" Boolean NonZero
  Refinement _ t _ -> error $ "Bad operand types for operator not: expected Bool, got (" ++ show t ++ ")"
tc (g, (Let (i, t) a c))
  | evalPred t (tc (g, a)) = tc ((i, t):g, c)
  | otherwise = error $ "Incompatible types in variable " ++ show i ++ ": expected " ++ show t ++ ", got " ++ show (tc (g, a))
tc (g, (Lambda (Arrow dom codom) i b))
  | codom == tc ((i, dom):g, b) = (Arrow dom codom)
  | otherwise = error $ "Incompatible types in function return: expected " ++ show codom ++ ", got " ++ show (tc ((i, dom):g, b))
tc (g, (App f a)) = 
  let tf = tc (g, f)
      ta = tc (g, a)
  in case tf of
    Arrow dom codom -> 
      if ta == dom 
        then codom 
        else error $ "Type mismatch: expected argument of type " ++ show dom ++ ", got " ++ show ta
    t -> error $ "Cannot apply to: " ++ show t
tc (g, (Let (i, t) a c))
  | t == tc (g, a) = tc ((i, t):g, c)
  | otherwise = error $ "Incompatible types in variable " ++ show i ++ ": expected " ++ show t ++ ", got " ++ show (tc (g, a))

evalPred :: Predicate -> Predicate -> Bool
evalPred NonZero MaybeZero = True
evalPred Zero MaybeZero = True
evalPred p1 p2 = p1 == p2

lookup :: Gamma -> String -> Type
lookup [] s = error "Free variable"
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s