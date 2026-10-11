{-|
Module      : Interp
Description : Intérprete de paso grande de Mini-Cepuac.

Semántica operacional de paso grande: @step (e, env)@ evalúa la expresión
@e@ bajo el ambiente @env@ directamente hasta un 'Value'.

El intérprete __confía en el verificador de tipos__ ("Checker"): nunca se
ejecuta un programa que no haya sido aceptado antes, así que aquí no se
revisan tipos, divisores ni índices. Los 'error' que aparecen son
defensivos y sólo podrían dispararse si se invocara el intérprete sin
verificar.

Las funciones se evalúan a cerraduras ('Closure') que capturan el ambiente
donde fueron definidas, lo que da alcance léxico. Las listas y los
registros son inmutables: @set@ y @with@ regresan copias.
-}
module Interp where
import Grammar
import Prelude hiding (lookup)
import Data.List (sortOn)

-- | Valores a los que se evalúan las expresiones.
data Value
  = IntV Integer                -- ^ entero de precisión arbitraria
  | DoubleV Double              -- ^ punto flotante
  | StrV String                 -- ^ cadena
  | BoolV Bool                  -- ^ booleano
  | ListV [Value]               -- ^ lista
  | RecV [(String, Value)]      -- ^ registro, campos ordenados por nombre
  | Closure String ASA Env      -- ^ parámetro, cuerpo y ambiente capturado

-- | Ambiente: valores de las variables en alcance (la primera aparición es
-- la visible).
type Env = [(String, Value)]

-- | Configuraciones del sistema de transiciones: expresión y ambiente.
type Config = (ASA, Env)

instance Show Value where
  show (IntV n) = show n
  show (DoubleV d) = show d
  show (StrV s) = show s
  show (BoolV True) = "#t"
  show (BoolV False) = "#f"
  show (ListV vs) = "[" ++ commas (map show vs) ++ "]"
  show (RecV fs) = "(record " ++ unwords ["(" ++ f ++ " " ++ show v ++ ")" | (f, v) <- fs] ++ ")"
  show (Closure x _ _) = "<lambda (" ++ x ++ ")>"

commas :: [String] -> String
commas [] = ""
commas [x] = x
commas (x : xs) = x ++ ", " ++ commas xs

-- | Relación de evaluación @(e, env) ⇓ v@.
step :: Config -> Value
-- literales
step (AInt n, _) = IntV n
step (ADouble d, _) = DoubleV d
step (AStr s, _) = StrV s
step (ABool b, _) = BoolV b
-- variables
step (Id x, env) = case lookup x env of
                      Just v  -> v
                      Nothing -> error ("Variable " ++ x ++ " not found")
-- aritmética: entera o de punto flotante según los operandos.
-- La división entera trunca hacia cero (quot) y el residuo lleva el signo
-- del dividendo (rem), igual que en C.
step (Add a b, env) = numOp (+) (+) (step (a, env)) (step (b, env))
step (Sub a b, env) = numOp (-) (-) (step (a, env)) (step (b, env))
step (Mul a b, env) = numOp (*) (*) (step (a, env)) (step (b, env))
step (Div a b, env) = numOp quot (/) (step (a, env)) (step (b, env))
step (Mod a b, env) = numOp rem (\_ _ -> error "% on double") (step (a, env)) (step (b, env))
-- cadenas y listas
step (Concat a b, env) = case (step (a, env), step (b, env)) of
  (StrV x, StrV y) -> StrV (x ++ y)
  (ListV x, ListV y) -> ListV (x ++ y)
  (x, y) -> error ("Cannot concatenate " ++ show x ++ " and " ++ show y)
step (Len a, env) = case step (a, env) of
  StrV s -> IntV (fromIntegral (length s))
  ListV vs -> IntV (fromIntegral (length vs))
  v -> error ("len of " ++ show v)
step (ListLit es, env) = ListV [step (e, env) | e <- es]
step (Cons x xs, env) = ListV (step (x, env) : unboxList (step (xs, env)))
step (Head xs, env) = case unboxList (step (xs, env)) of
  (v : _) -> v
  [] -> error "head of empty list"
step (Tail xs, env) = ListV (drop 1 (unboxList (step (xs, env))))
step (Nth xs i, env) = unboxList (step (xs, env)) !! fromIntegral (unboxInt (step (i, env)))
step (SetNth xs i v, env) =
  let (before, _ : after) = splitAt (fromIntegral (unboxInt (step (i, env)))) (unboxList (step (xs, env)))
  in ListV (before ++ step (v, env) : after)
step (Replicate n v, env) = ListV (replicate (fromIntegral (unboxInt (step (n, env)))) (step (v, env)))
-- registros
step (Record fs, env) = RecV (sortOn fst [(f, step (v, env)) | (f, v) <- fs])
step (Get r f, env) = case lookup f (unboxRec (step (r, env))) of
  Just v -> v
  Nothing -> error ("Record has no field " ++ f)
step (With r f v, env) =
  let v' = step (v, env)
  in RecV [(f', if f' == f then v' else old) | (f', old) <- unboxRec (step (r, env))]
-- conversiones
step (ToInt a, env) = IntV (truncate (unboxDouble (step (a, env))))
step (ToDouble a, env) = DoubleV (fromIntegral (unboxInt (step (a, env))))
-- lógica y comparaciones
step (Not b, env) = BoolV (not (unboxBool (step (b, env))))
step (And a b, env) = BoolV (unboxBool (step (a, env)) && unboxBool (step (b, env)))
step (Or a b, env) = BoolV (unboxBool (step (a, env)) || unboxBool (step (b, env)))
step (Cmp op a b, env) = BoolV (compareV op (step (a, env)) (step (b, env)))
-- condicional: sólo se evalúa la rama elegida
step (If c t e, env) = if unboxBool (step (c, env)) then step (t, env) else step (e, env)
-- let: se evalúa el valor y se extiende el ambiente para el cuerpo
step (Let (x, _) a b, env) = let a' = step (a, env)
  in step (b, (x, a') : env)
-- letrec: el ambiente extendido se usa también para evaluar el valor
-- (nudo recursivo por pereza de Haskell; funciona cuando el valor es una
-- lambda, que es el caso de toda definición recursiva)
step (Letrec (x, _) a b, env) = let env' = (x, a') : env
                                    a' = step (a, env')
  in step (b, env')
-- lambda: cerradura con el ambiente actual
step (Lambda _ x b, env) = Closure x b env
step (LambdaN _ xs _, _) = error ("Internal: lambda (" ++ unwords xs ++ ") was not desugared")
-- aplicación: se evalúa el argumento (paso por valor) y el cuerpo de la
-- cerradura en su ambiente capturado, extendido con el parámetro
step (App f a, env) = case step (f, env) of
    Closure x b env' -> let a' = step (a, env) in step (b, (x, a') : env')
    v -> error ("Cannot apply non-function " ++ show v)

-- | Aplica la operación entera o la de punto flotante según los operandos.
numOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double)
      -> Value -> Value -> Value
numOp fi _ (IntV a) (IntV b) = IntV (fi a b)
numOp _ fd (DoubleV a) (DoubleV b) = DoubleV (fd a b)
numOp _ _ a b = error ("Mixed numeric operands: " ++ show a ++ ", " ++ show b)

-- | Comparación entre valores del mismo tipo.
compareV :: CmpOp -> Value -> Value -> Bool
compareV op (IntV a) (IntV b) = cmpWith op a b
compareV op (DoubleV a) (DoubleV b) = cmpWith op a b
compareV op (StrV a) (StrV b) = cmpWith op a b
compareV op (BoolV a) (BoolV b) = cmpWith op a b
compareV _ a b = error ("Cannot compare " ++ show a ++ " with " ++ show b)

cmpWith :: Ord a => CmpOp -> a -> a -> Bool
cmpWith Eq = (==)
cmpWith Neq = (/=)
cmpWith Lt = (<)
cmpWith Le = (<=)
cmpWith Gt = (>)
cmpWith Ge = (>=)

-- Proyecciones de 'Value'. Fallan sólo si el programa no fue verificado.

unboxInt :: Value -> Integer
unboxInt (IntV n) = n
unboxInt v = error ("Expected int, got " ++ show v)

unboxDouble :: Value -> Double
unboxDouble (DoubleV d) = d
unboxDouble v = error ("Expected double, got " ++ show v)

unboxStr :: Value -> String
unboxStr (StrV s) = s
unboxStr v = error ("Expected string, got " ++ show v)

unboxBool :: Value -> Bool
unboxBool (BoolV b) = b
unboxBool v = error ("Expected boolean, got " ++ show v)

unboxList :: Value -> [Value]
unboxList (ListV vs) = vs
unboxList v = error ("Expected list, got " ++ show v)

unboxRec :: Value -> [(String, Value)]
unboxRec (RecV fs) = fs
unboxRec v = error ("Expected record, got " ++ show v)

-- | Valor de una variable en el ambiente.
lookup :: String -> [(String, a)] -> Maybe a
lookup _ [] = Nothing
lookup x ((y, v):env)
  | x == y    = Just v
  | otherwise = lookup x env

-- | Evalúa un programa cerrado (sin variables libres).
interp :: ASA -> Value
interp e = step (e, [])
