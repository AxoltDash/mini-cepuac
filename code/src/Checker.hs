{-# LANGUAGE BangPatterns #-}
module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- COnfiguraciones del sistema de transicion. (revisar)

tc :: ConfigT -> Type
tc (_, (ANum n)) 
              | n == 0 = Refinement Number Zero
              | otherwise = Refinement Number NonZero
tc (_, (ABool b)) = Refinement Boolean NonZero
tc (g, Id s) = lookup g s
tc (g, (Add i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number Zero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number NonZero, Refinement Number Zero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator + : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Sub i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number Zero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number NonZero, Refinement Number Zero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator - : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Mul i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number NonZero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number _) -> Refinement Number Zero
  (Refinement Number _, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator * : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Div i d)) = case (tc (g, i), tc (g, d)) of
  (_, Refinement Number Zero) -> error $ "This expression contain a division by zero."
  (Refinement Number NonZero, Refinement Number _) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number _) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator / : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (And i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Boolean _, Refinement Boolean _) -> Refinement Boolean NonZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator && : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Or i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Boolean _, Refinement Boolean _) -> Refinement Boolean NonZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator || : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Not b)) = case tc (g, b) of
  Refinement Boolean _ -> Refinement Boolean NonZero
  Refinement t _ -> error $ "Bad operand types for operator not: expected Bool, got (" ++ show t ++ ")"
tc (g, (Let (i, r) v b)) = let !r' = solve r
                               !rv = tc (g, v) 
                               !e = evalr rv r'
                                in tc ((i, rv) : g, b)
tc (g, (Lambda (Arrow dom codom) i b)) = let !dom' = solve dom 
                                             !codom' = solve codom
                                              in Arrow dom' codom'
tc (g, (App f a)) = case tc (g, f) of 
                    (Arrow dom codom) -> let !t = tc (g, a) 
                                             !e = evalr t dom
                                              in codom  
                    _ -> error "Cannot apply to a non-function"

solve :: Type -> Type
solve (Refinement Number p) = Refinement Number p 
solve (Refinement Boolean p) = Refinement Boolean p 
solve (Arrow dom codom) = let !dom' = solve dom 
                              !codom' = solve codom 
                            in Arrow dom' codom'
solve (Refinement r p) = let Refinement t p' = solve r in 
     if (evalPred p' p) 
     then (Refinement t p') 
     else error $ "Predicate mismatch: expected " ++ show p ++ ", got " ++ show p' ++ " in type " ++ show t

evalr :: Type -> Type -> Type
evalr (Refinement t1 p1) (Refinement t2 p2) = 
  if ((t1 == t2) && (evalPred p1 p2)) 
    then (Refinement t1 p1) 
    else error $ "Type or predicate mismatch: expected type " ++ show t2 ++ " with predicate " ++ show p2 ++ ", got type " ++ show t1 ++ " with predicate " ++ show p1
evalr (Arrow dom1 codom1) (Arrow dom2 codom2) = let !dom = evalr dom1 dom2 
                                                    !codom = evalr codom1 codom2 
                                                  in Arrow dom codom
evalr (Refinement _ _) (Arrow dom codom) = error $ "Type mismatch: cannot match refinement type with function type (Arrow " ++ show dom ++ " " ++ show codom ++ ")"
evalr (Arrow dom codom) (Refinement _ _) = error $ "Type mismatch: cannot match function type (Arrow " ++ show dom ++ " " ++ show codom ++ ") with refinement type"

evalPred :: Predicate -> Predicate -> Bool
evalPred NonZero MaybeZero = True
evalPred Zero MaybeZero = True
evalPred p1 p2 = p1 == p2

lookup :: Gamma -> String -> Type
lookup [] s = error "Free variable"
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s
