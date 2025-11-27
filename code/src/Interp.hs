{-# LANGUAGE BangPatterns #-}
module Interp where
import Desugar hiding (lookup)
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
  | NilV
  | LinV
  | PairV Value Value 
  | ClosureV

-- Configuraciones del sistema de transiciones
type Config = (ASA, Env, Tag) 

-- Ambiente sobre el cual se evalua una expresion
type Env = [(String, ASA, Tag)]

instance Show Value where
  show (NumV n) = show n
  show (BooleanV True) = "#t"
  show (BooleanV False) = "#f"
  show (NilV) = "[]"
  show (PairV LinV (PairV x y)) = "(" ++ show x ++ "," ++ show y ++ ")"
  show (PairV x y) = show $ auxshow (PairV x y)
      where 
        auxshow :: Value -> [Value]
        auxshow (PairV x NilV) = [x]
        auxshow (PairV x y) = x : auxshow y 
  show (ClosureV) = "<procedure>"

toValue :: ASA -> Value
toValue (Num n) = NumV n
toValue (Boolean b) = BooleanV b 
toValue (Nil) = NilV
toValue (Lin) = LinV
toValue (Pair x y) = PairV (toValue x) (toValue y)
toValue (Lambda _ _) = ClosureV

step :: Config -> Config
step (Num n, env, ASAT) = (Num n, env, Value) 
step (Lin, env, ASAT) = (Lin, env, Value)
step (Boolean b, env, ASAT) = (Boolean b, env, Value) 
step (Nil, env, ASAT) = (Nil, env, Value)
step (Pair _ (Pair Lin _), _, _) = error "Cannot append to a Pair"
step (Pair x y, env, ASAT) = (Pair (getASA $ strict (x, env, ASAT)) (getASA $ strict (y, env, ASAT)), env, Value)
step (Lambda v b, env, ASAT) = (Lambda v b, env, Closure env) -- La etiqueta (Tag) captura el ambiente de definicion

step (Var x, env, ASAT) = case lookup x env of
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

step (Pow (Num a) (Num b), env, ASAT) = (Num (a ** b), env, Value)
step (Pow (Num a) b, env, ASAT) = (Pow (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Pow a b, env, ASAT) = (Pow (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (Sqrt (Num a), env, ASAT)
    | a < 0 = error "You can't take the square root of a number less than 0"
    | otherwise = (Num (sqrt a), env, Value)
step (Sqrt a, env, ASAT) = (Sqrt (getASA $ step (a, env, ASAT)), env, ASAT)

step (If (Boolean True) t e, env, ASAT) = (t, env, ASAT)
step (If (Boolean False) t e, env, ASAT) = (e, env, ASAT)
step (If cond t e, env, ASAT) = (If (getASA $ step (cond, env, ASAT)) t e, env, ASAT)

step (Gt (Num a) (Num b), env, ASAT) = (Boolean (a > b), env, Value)
step (Gt (Num a) b, env, ASAT) = (Gt (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Gt a b, env, ASAT) = (Gt (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (Eq a b, env, ASAT) = case step (a, env, ASAT) of
                          (a', _, ASAT) -> (Eq a' b, env, ASAT)
                          (a', _, _) -> case step (b, env, ASAT) of -- Eq es polimorfico
                                            (b', _, ASAT) -> (Eq a' b', env, ASAT)
                                            (b', _, Value) -> (Boolean (a' == b'), env, Value)
                                            (b', _, Closure _) -> (Boolean False, env, Value)

step (Geq (Num a) (Num b), env, ASAT) = (Boolean (a >= b), env, Value)
step (Geq (Num a) b, env, ASAT) = (Geq (Num a) (getASA $ step (b, env, ASAT)), env, ASAT)
step (Geq a b, env, ASAT) = (Geq (getASA $ step (a, env, ASAT)) b, env, ASAT)

step (Not (Boolean b), env, ASAT) = (Boolean (not b), env, Value)
step (Not b, env, ASAT) = (Not (getASA $ step (b, env, ASAT)), env, ASAT)

step (Fst (Pair Lin Nil), env, ASAT) = error "Empty list"
step (Fst (Pair Lin (Pair x y)), env, ASAT) = let (Pair Lin (Pair x' y'), env', tag') = step (Pair Lin (Pair x y), env, ASAT) in (x', env', Value)
step (Fst p, env, ASAT) = (Fst (getASA $ step (p, env, ASAT)), env, ASAT) 

step (Snd (Pair Lin Nil), env, ASAT) = (Nil, env, Value)
step (Snd (Pair Lin (Pair x y)), env, ASAT) = let (Pair Lin (Pair x' y'), env', tag') = step (Pair Lin (Pair x y), env, ASAT) in (y', env', Value)
step (Snd p, env, ASAT) = (Snd (getASA $ step (p, env, ASAT)), env, ASAT) 

step (App f a, env, ASAT) = case step (f, env, ASAT) of
                (Lambda v b, _, Closure env') -> case step (a, env, ASAT) of -- refleja el enfoque ansioso de cepuac
                        (a', _, ASAT) ->  (App f a', env, ASAT)
                        (a', _, tag) -> let (b', _, tag') = strict (b, (v, a', tag):env', ASAT) in -- aisla la evalucion del cuerpo en el ambiente de la cerradura
                            (b', env, tag') 
                (f', _, ASAT) -> (App f' a, env, ASAT)
                (f', _, Value) -> error ("Cannot apply to:" ++ show (toValue f')) -- f no se puede reducir y no es una lambda

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
