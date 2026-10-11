{-# LANGUAGE BangPatterns #-}
{-|
Module      : Checker
Description : Verificador de tipos con refinamientos de Mini-Cepuac.

== Qué hace este módulo

Decide, antes de ejecutar un programa, si está bien tipado. Trabaja con
juicios de la forma

>   Γ ⊢ e : T

que se leen "bajo el contexto Γ, la expresión e tiene tipo T". Aquí T es un
'RType': un tipo base junto con el __conjunto de valores__ que la expresión
puede tomar ('R'), una lista con el conjunto de longitudes posibles ('List'),
un registro ('Rec') o un tipo función ('Fn').

El verificador hace dos cosas a la vez:

* __Infiere__ el refinamiento más preciso que puede justificar para cada
  expresión. Por ejemplo, @(+ 3 5)@ tiene tipo @{ v : int | v == 8 }@ y
  @(* y 2)@ con @y ∈ [1,10]@ tiene tipo @{ v : int | v >= 2 && v <= 20 }@.
* __Comprueba__ que lo inferido sea un subtipo de lo que el programador anotó
  en @let@, @letrec@ y @lambda@, y que las operaciones parciales (@/@, @%@,
  @head@, @tail@, @nth@, @set@) sean seguras.

== Cómo se representan los refinamientos

Un predicado como @v > 0 && v != 5@ se traduce (función 'denote') a un
conjunto de intervalos 'ISet' del módulo "Refine". Con esa representación:

* la implicación @p ⇒ q@ es simplemente @[[p]] ⊆ [[q]]@ ('subset');
* la unión de ramas de un @if@ es la unión de conjuntos;
* la aritmética sobre expresiones es aritmética de intervalos.

Cada tipo se "mide" en la recta real de forma distinta ('fitBase'):

>   int     -> el propio valor, restringido a enteros
>   double  -> el propio valor
>   string  -> su longitud (un entero >= 0)
>   list T  -> su longitud (un entero >= 0)
>   boolean -> 0 para #f, 1 para #t

Los registros y las funciones no tienen refinamiento propio; sus
componentes sí.

== Organización del módulo

1. Tipos del verificador ('RType', 'Gamma').
2. Reglas de tipado ('tc'), una ecuación por constructor del ASA.
3. Auxiliares de las reglas (aritmética, división segura, índices, booleanos).
4. Subtipado ('sub'), comprobación ('check') y unión ('join').
5. Normalización de anotaciones ('normType', 'denote').
6. Tipado sensible al flujo para @if@ ('assume').
7. Contexto e impresión legible de tipos ('showR', 'reify').
-}
module Checker where

import Grammar
import Prelude hiding (lookup)
import Refine
import Data.List (intercalate, sortOn)

-- * Tipos del verificador

-- | Tipo normalizado. A diferencia de 'Type' (lo que escribe el programador,
-- con refinamientos anidados y predicados sintácticos), aquí cada tipo
-- lleva ya su conjunto de valores calculado.
--
-- Los campos son estrictos para que cualquier error de tipos escondido en el
-- cálculo del conjunto (por ejemplo una división entre cero) salga a la
-- superficie en cuanto se construye el tipo.
data RType
  = R !Base !ISet           -- ^ tipo base + conjunto de valores (medidos) posibles
  | List !RType !ISet       -- ^ lista: tipo de los elementos + longitudes posibles
  | Rec ![(String, RType)]  -- ^ registro: campos ordenados por nombre
  | Fn !RType !RType        -- ^ tipo función: dominio -> codominio
  | Bot                     -- ^ tipo vacío: elementos de la lista vacía @(list)@
  deriving (Eq)

instance Show RType where
  show = showR

-- | Contexto de tipificado Γ: asocia cada variable visible con su tipo.
-- La primera aparición es la que está en alcance (permite sombreado).
type Gamma = [(String, RType)]

-- | Configuraciones del sistema de tipos: un contexto y la expresión a tipar.
type ConfigT = (Gamma, ASA)

-- * Reglas de tipado

-- | @tc (Γ, e)@ calcula T tal que Γ ⊢ e : T, o aborta con 'error' si la
-- expresión no está bien tipada. Hay una ecuación por cada constructor del
-- ASA; los comentarios de cada una muestran la regla que implementa.
tc :: ConfigT -> RType

-- Literales. El refinamiento más preciso de un literal es el propio valor:
--
--   Γ ⊢ 3 : { v : int | v == 3 }       Γ ⊢ "ab" : { v : string | len v == 2 }
tc (_, AInt n)    = R TInt (single (fromIntegral n))
tc (_, ADouble d) = R TDouble (single d)
tc (_, AStr s)    = R TString (single (fromIntegral (length s)))
tc (_, ABool b)   = R TBool (single (bit b))

-- Variables: se consulta el contexto.
--
--   (x : T) ∈ Γ
--   -----------
--   Γ ⊢ x : T
tc (g, Id x)      = lookup g x

-- Aritmética. Ambos operandos deben ser del mismo tipo numérico y el
-- refinamiento del resultado se calcula con aritmética de intervalos:
--
--   Γ ⊢ a : { v : int | v ∈ A }    Γ ⊢ b : { v : int | v ∈ B }
--   -----------------------------------------------------------
--   Γ ⊢ (+ a b) : { v : int | v ∈ A ⊕ B }        (igual para - y *)
tc (g, Add a b) = arith "+" addSet addSet g a b
tc (g, Sub a b) = arith "-" subSet subSet g a b
tc (g, Mul a b) = arith "*" mulSet mulSet g a b

-- División y residuo. Además de lo anterior se exige 0 ∉ B: si el
-- refinamiento del divisor admite al cero no hay prueba de que la operación
-- sea segura y el programa se rechaza ('nonZero').
tc (g, Div a b) = case (tc (g, a), tc (g, b)) of
  (R TInt sa, R TInt sb)       -> R TInt (quotSet sa (nonZero "/" TInt sb))
  (R TDouble sa, R TDouble sb) -> R TDouble (divSet sa (nonZero "/" TDouble sb))
  (ta, tb) -> badOperands "/" "two int or two double" [ta, tb]
tc (g, Mod a b) = case (tc (g, a), tc (g, b)) of
  (R TInt sa, R TInt sb) -> R TInt (remSet sa (nonZero "%" TInt sb))
  (ta, tb) -> badOperands "%" "two int" [ta, tb]

-- Cadenas y listas. Su refinamiento describe la longitud, así que la
-- concatenación suma longitudes y `len` convierte ese conjunto en un int:
--
--   Γ ⊢ a : { v : string | len v ∈ A }   Γ ⊢ b : { v : string | len v ∈ B }
--   ----------------------------------------------------------------------
--   Γ ⊢ (++ a b) : { v : string | len v ∈ A ⊕ B }
tc (g, Concat a b) = case (tc (g, a), tc (g, b)) of
  (R TString sa, R TString sb) -> R TString (addSet sa sb)
  (List ea sa, List eb sb)     -> List (join ea eb) (fitLen (addSet sa sb))
  (ta, tb) -> badOperands "++" "two string or two list" [ta, tb]
tc (g, Len a) = case tc (g, a) of
  R TString s -> R TInt s
  List _ s    -> R TInt s
  ta -> badOperands "len" "string or list" [ta]

-- Listas. La lista literal tiene longitud exacta y como tipo de elementos la
-- unión de los tipos de sus elementos (Bot si está vacía). cons suma 1 a
-- la longitud; head y tail exigen que la lista no pueda ser vacía:
--
--   Γ ⊢ xs : { v : list T | len v ∈ S }    0 ∉ S
--   ----------------------------------------------
--   Γ ⊢ (head xs) : T
tc (g, ListLit es) = List (foldr join Bot (map (\e -> tc (g, e)) es))
                          (single (fromIntegral (length es)))
tc (g, Cons x xs) = case tc (g, xs) of
  List e s -> List (join (tc (g, x)) e) (fitLen (addSet s (single 1)))
  t -> badOperands "cons" "a value and a list" [tc (g, x), t]
tc (g, Head xs) = case tc (g, xs) of
  List e s -> case nonEmpty "head" e s of () -> e
  t -> badOperands "head" "list" [t]
tc (g, Tail xs) = case tc (g, xs) of
  List e s -> case nonEmpty "tail" e s of () -> List e (fitLen (subSet s (single 1)))
  t -> badOperands "tail" "list" [t]

-- Acceso por índice. El índice debe estar probadamente en rango: su conjunto
-- de valores debe caber en [0, inf(S) - 1], donde S son las longitudes
-- posibles de la lista:
--
--   Γ ⊢ xs : { v : list T | len v ∈ S }   Γ ⊢ i : { v : int | v ∈ I }   I ⊆ [0, inf S)
--   ----------------------------------------------------------------------------
--   Γ ⊢ (nth xs i) : T
tc (g, Nth xs i) = case (tc (g, xs), tc (g, i)) of
  (List e s, R TInt si) -> case inRange "nth" e s si of () -> e
  (t, ti) -> badOperands "nth" "a list and an int" [t, ti]
tc (g, SetNth xs i v) = case (tc (g, xs), tc (g, i)) of
  (List e s, R TInt si) -> case inRange "set" e s si of
                             () -> List (join e (tc (g, v))) s
  (t, ti) -> badOperands "set" "a list and an int" [t, ti]
tc (g, Replicate n v) = case tc (g, n) of
  R TInt sn | subset sn (cmpSet CGe 0) -> List (tc (g, v)) (fitLen sn)
            | otherwise -> error $ "replicate needs a non-negative count, got "
                           ++ showR (R TInt sn)
  t -> badOperands "replicate" "an int and a value" [t]

-- Registros. Los campos se guardan ordenados por nombre para que la
-- igualdad y el subtipado no dependan del orden en que se escribieron.
tc (g, Record fs)
  | hasDup (map fst fs) = error $ "Duplicate field in record: " ++ show (map fst fs)
  | otherwise = Rec (sortOn fst [(f, tc (g, v)) | (f, v) <- fs])
tc (g, Get r f) = case tc (g, r) of
  Rec fs -> field f fs
  t -> badOperands ("get " ++ f) "record" [t]
tc (g, With r f v) = case tc (g, r) of
  Rec fs -> field f fs `seq` Rec [(f', if f' == f then tc (g, v) else t) | (f', t) <- fs]
  t -> badOperands ("with " ++ f) "record" [t]

-- Conversiones explícitas entre int y double. toDouble conserva el conjunto;
-- toInt trunca hacia cero (por eso { v : double | v >= 0.5 } se convierte en
-- { v : int | v >= 0 }, que admite el cero).
tc (g, ToDouble a) = case tc (g, a) of
  R TInt s -> R TDouble s
  ta -> badOperands "toDouble" "int" [ta]
tc (g, ToInt a) = case tc (g, a) of
  R TDouble s -> R TInt (truncSet s)
  ta -> badOperands "toInt" "double" [ta]

-- Lógica. Se evalúa la tabla de verdad sobre los valores que cada operando
-- puede tomar; si ambos son constantes el resultado también lo es.
tc (g, And a b) = case (tc (g, a), tc (g, b)) of
  (R TBool sa, R TBool sb) -> R TBool (bools [x && y | x <- bvals sa, y <- bvals sb])
  (ta, tb) -> badOperands "&&" "two boolean" [ta, tb]
tc (g, Or a b) = case (tc (g, a), tc (g, b)) of
  (R TBool sa, R TBool sb) -> R TBool (bools [x || y | x <- bvals sa, y <- bvals sb])
  (ta, tb) -> badOperands "||" "two boolean" [ta, tb]
tc (g, Not a) = case tc (g, a) of
  R TBool s -> R TBool (bools [not x | x <- bvals s])
  ta -> badOperands "not" "boolean" [ta]

-- Comparaciones. Ambos lados deben tener el mismo tipo base; string y
-- boolean sólo admiten == y !=. Si los conjuntos ya deciden el resultado
-- (por ejemplo A = [1,3] y B = [5,9] para <) el booleano es un singulete.
tc (g, Cmp op a b) = case (tc (g, a), tc (g, b)) of
  (R ba sa, R bb sb)
    | ba /= bb -> badOperands (show op) "two values of the same type" [R ba sa, R bb sb]
    | ba `elem` [TString, TBool] && op `notElem` [Eq, Neq] ->
        error $ "Operator " ++ show op ++ " is not defined on " ++ show ba
    | otherwise -> R TBool (cmpOutcomes ba op sa sb)
  (ta, tb) -> badOperands (show op) "two values of the same base type" [ta, tb]

-- Condicional con tipado sensible al flujo:
--
--   Γ ⊢ c : boolean    Γ ∧ c ⊢ t : T₁    Γ ∧ ¬c ⊢ e : T₂
--   ----------------------------------------------------
--   Γ ⊢ (if c t e) : T₁ ∪ T₂
--
-- "Γ ∧ c" es el contexto donde las variables mencionadas en c se refinan
-- con lo que c afirma de ellas ('assume'). Así, en
-- (if (!= y 0) (/ 10 y) 0) la rama then ve y : { v : int | v != 0 } y la
-- división es segura. Si la condición es constante sólo cuenta la rama
-- alcanzable, aunque ambas se verifican.
tc (g, If c t e) = case tc (g, c) of
  R TBool sc ->
    let !tt = tc (assume g c True, t)
        !te = tc (assume g c False, e)
    in case bvals sc of
         [True]  -> tt
         [False] -> te
         _       -> join tt te
  tcond -> error $ "Condition of if must be boolean, got " ++ showR tcond

-- Let, con o sin anotación:
--
--   Γ ⊢ v : T    T <: R    Γ, x : T ⊢ b : B
--   ----------------------------------------
--   Γ ⊢ (let (x : R v) b) : B
--
-- La anotación R funciona como contrato que se verifica; dentro del cuerpo
-- la variable conserva el tipo T, más preciso, que se infirió para el valor.
-- Por eso (let (x : int 5) (/ 1 x)) es aceptado: x : { v : int | v == 5 }.
-- Sin anotación simplemente se usa T.
tc (g, Let (x, ann) v b) =
  let tv = tc (g, v)
      ok = case ann of
        Just r  -> check ("value bound to " ++ x) tv (normType r)
        Nothing -> ()
  in case ok of
       () -> tc ((x, tv) : g, b)

-- Letrec: la variable es visible dentro de su propio valor, con el tipo
-- anotado (es el único tipo que se puede suponer antes de verificar el
-- valor). Es la base de las definiciones recursivas.
--
--   Γ, x : R ⊢ v : T    T <: R    Γ, x : T ⊢ b : B
--   -----------------------------------------------
--   Γ ⊢ (letrec (x : R v) b) : B
tc (g, Letrec (x, r) v b) =
  let r' = normType r
      tv = tc ((x, r') : g, v)
  in case check ("value bound to " ++ x) tv r' of
       () -> tc ((x, tv) : g, b)

-- Lambda con anotación de tipo función:
--
--   Γ, x : D ⊢ b : T    T <: C
--   ---------------------------------------
--   Γ ⊢ (lambda : D -> C (x) b) : D -> C
--
-- El cuerpo se verifica al definir la función, suponiendo únicamente lo que
-- el dominio promete del parámetro. (lambda : int -> int (y) (/ 10 y)) se
-- rechaza porque int admite el cero.
tc (g, Lambda ann x b) = case normType ann of
  Fn dom codom ->
    case check ("body of lambda (" ++ x ++ ")") (tc ((x, dom) : g, b)) codom of
      () -> Fn dom codom
  t -> error $ "Lambda must be annotated with a function type, got " ++ showR t
tc (_, LambdaN _ xs _) = error $ "Internal: lambda (" ++ unwords xs ++ ") was not desugared"

-- Aplicación:
--
--   Γ ⊢ f : D -> C    Γ ⊢ a : T    T <: D
--   --------------------------------------
--   Γ ⊢ (f a) : C
tc (g, App f a) = case tc (g, f) of
  Fn dom codom ->
    case check "function argument" (tc (g, a)) dom of
      () -> codom
  t -> error $ "Cannot apply a non-function of type " ++ showR t

-- * Auxiliares de las reglas

-- | Regla común de @+@, @-@ y @*@. Recibe el nombre del operador (para los
-- mensajes de error) y la operación de intervalos para int y para double.
-- El resultado entero se vuelve a ajustar a enteros con 'intNorm'.
arith :: String -> (ISet -> ISet -> ISet) -> (ISet -> ISet -> ISet)
      -> Gamma -> ASA -> ASA -> RType
arith name opI opD g a b = case (tc (g, a), tc (g, b)) of
  (R TInt sa, R TInt sb)       -> R TInt (intNorm (opI sa sb))
  (R TDouble sa, R TDouble sb) -> R TDouble (opD sa sb)
  (ta, tb) -> badOperands name "two int or two double (use toInt/toDouble to convert)" [ta, tb]

-- | Comprueba que el conjunto de valores de un divisor excluya al cero.
-- Convierte un error en tiempo de ejecución (dividir entre cero) en un
-- error de tipos.
nonZero :: String -> Base -> ISet -> ISet
nonZero op b s
  | member 0 s = error $ "Possible division by zero: the divisor of " ++ op
                 ++ " has type " ++ showR (R b s) ++ ", which admits 0"
  | otherwise = s

-- | Comprueba que una lista no pueda ser vacía (para @head@ y @tail@).
nonEmpty :: String -> RType -> ISet -> ()
nonEmpty op e s
  | member 0 s = error $ "Possible " ++ op ++ " of an empty list: the argument has type "
                 ++ showR (List e s) ++ ", which admits length 0"
  | otherwise = ()

-- | Comprueba que un índice esté en rango para todas las longitudes posibles
-- de la lista: @I ⊆ [0, inf S - 1]@.
inRange :: String -> RType -> ISet -> ISet -> ()
inRange op e s si
  | subset si [mk 0 True (infimum s - 1) True] = ()
  | otherwise = error $ "Index possibly out of range in " ++ op ++ ": index has type "
                ++ showR (R TInt si) ++ " but the list has type " ++ showR (List e s)

-- | Tipo de un campo de un registro.
field :: String -> [(String, RType)] -> RType
field f fs = case lookupMaybe fs f of
  Just t  -> t
  Nothing -> error $ "Record has no field " ++ f ++ ": " ++ showR (Rec fs)

hasDup :: [String] -> Bool
hasDup [] = False
hasDup (x : xs) = x `elem` xs || hasDup xs

-- | Error uniforme para operandos de tipo incorrecto.
badOperands :: String -> String -> [RType] -> a
badOperands op expected got = error $
  "Bad operand types for operator " ++ op ++ ": expected " ++ expected
  ++ ", got (" ++ intercalate ", " (map showR got) ++ ")"

-- | Medida de un booleano en la recta: #f ↦ 0, #t ↦ 1.
bit :: Bool -> Double
bit True = 1
bit False = 0

-- | Conjunto de valores de un boolean a partir de los resultados posibles.
bools :: [Bool] -> ISet
bools bs = fitBase TBool (foldr (union . single . bit) bot bs)

-- | Valores booleanos posibles de un refinamiento sobre boolean
-- (inverso de 'bools').
bvals :: ISet -> [Bool]
bvals s = [b | b <- [False, True], member (bit b) s]

-- | Resultados posibles de comparar un valor de A con uno de B.
--
-- @True@ es posible si existen a ∈ A, b ∈ B con @a op b@; @False@ es posible
-- si existen a, b con @a (¬op) b@. Para strings los conjuntos describen
-- longitudes: dos cadenas de la misma longitud pueden ser distintas, salvo
-- que la longitud sea 0.
cmpOutcomes :: Base -> CmpOp -> ISet -> ISet -> ISet
cmpOutcomes b op sa sb
  | isEmpty sa || isEmpty sb = bot
  | otherwise = bools [r | r <- [False, True], possible (if r then op else negCmp op)]
  where
    possible Eq  = not (isEmpty (inter sa sb))
    possible Neq = case (asSingleton sa, asSingleton sb) of
                     (Just x, Just y) -> x /= y || b == TString && x > 0
                     _ -> True
    possible Lt  = infimum sa < supremum sb
    possible Gt  = supremum sa > infimum sb
    possible Le  = possible Lt || possible Eq
    possible Ge  = possible Gt || possible Eq

-- * Subtipado y unión

-- | Relación de subtipado @T₁ <: T₂@:
--
-- * Entre refinamientos del mismo tipo base es inclusión de conjuntos,
--   es decir, implicación de predicados.
-- * Las listas son covariantes en los elementos (son inmutables) y sus
--   longitudes se incluyen.
-- * Los registros deben tener los mismos campos, cada uno covariante.
-- * Entre funciones es __contravariante__ en el dominio y __covariante__ en el
--   codominio: una función que acepta más y promete más sirve donde se
--   esperaba una más restrictiva.
-- * 'Bot' es subtipo de todo (no tiene valores).
--
-- >   S₁ ⊆ S₂                    D₂ <: D₁    C₁ <: C₂
-- >   -------------------        ------------------------
-- >   {v:b | S₁} <: {v:b | S₂}   D₁ -> C₁ <: D₂ -> C₂
sub :: RType -> RType -> Bool
sub Bot _ = True
sub (R b1 s1) (R b2 s2) = b1 == b2 && subset s1 s2
sub (List e1 s1) (List e2 s2) = sub e1 e2 && subset s1 s2
sub (Rec f1) (Rec f2) = map fst f1 == map fst f2 && and (zipWith sub (map snd f1) (map snd f2))
sub (Fn d1 c1) (Fn d2 c2) = sub d2 d1 && sub c1 c2
sub _ _ = False

-- | Exige @actual <: expected@; @what@ describe el lugar para el mensaje.
check :: String -> RType -> RType -> ()
check what actual expected
  | sub actual expected = ()
  | otherwise = error $ "Type mismatch in " ++ what ++ ": expected "
                ++ showR expected ++ ", got " ++ showR actual

-- | Tipo de la unión de dos ramas (o de dos elementos de una lista). Para
-- refinamientos del mismo tipo base es la unión de conjuntos; listas y
-- registros se unen componente a componente; para funciones se exige que
-- una sea subtipo de la otra (no se calcula el supremo de tipos función).
join :: RType -> RType -> RType
join Bot t = t
join t Bot = t
join (R b1 s1) (R b2 s2)
  | b1 == b2 = R b1 (fitBase b1 (union s1 s2))
join (List e1 s1) (List e2 s2) = List (join e1 e2) (fitLen (union s1 s2))
join (Rec f1) (Rec f2)
  | map fst f1 == map fst f2 = Rec [(f, join t1 t2) | ((f, t1), (_, t2)) <- zip f1 f2]
join t1 t2
  | sub t1 t2 = t2
  | sub t2 t1 = t1
  | otherwise = error $ "Incompatible types: " ++ showR t1 ++ " and " ++ showR t2

-- * Normalización de tipos anotados

-- | Ajusta un conjunto al dominio de la medida de cada tipo base: los
-- enteros se redondean hacia adentro, las longitudes no son negativas y los
-- booleanos viven en {0, 1}. Toda operación sobre un @R b s@ pasa por aquí
-- para mantener los conjuntos en forma canónica.
fitBase :: Base -> ISet -> ISet
fitBase TInt    = intNorm
fitBase TDouble = id
fitBase TString = fitLen
fitBase TBool   = intNorm . inter [mk 0 True 1 True]

-- | Ajuste para longitudes (strings y listas): enteros no negativos.
fitLen :: ISet -> ISet
fitLen = intNorm . inter [mk 0 True inf False]

-- | Traduce un tipo escrito por el programador a su forma normal:
--
-- * un tipo base sin refinar es el conjunto completo de su medida;
-- * listas, registros y flechas se normalizan componente a componente;
-- * @{ v : T | p }@ intersecta el conjunto de T con la denotación de p, por
--   lo que los refinamientos anidados se aplanan por conjunción;
-- * refinar un registro o un tipo función es un error.
normType :: Type -> RType
normType (TBase b) = R b (fitBase b top)
normType (TName n) = error $ "Internal: type alias " ++ n ++ " was not expanded"
normType (TList t) = List (normType t) (fitLen top)
normType (TRecord fs)
  | hasDup (map fst fs) = error $ "Duplicate field in record type: " ++ show (TRecord fs)
  | otherwise = Rec (sortOn fst [(f, normType t) | (f, t) <- fs])
normType (Arrow d c) = Fn (normType d) (normType c)
normType (Refinement v t p) = case normType t of
  R b s    -> R b (fitBase b (inter s (denote (Just b) v p)))
  List e s -> List e (fitLen (inter s (denote Nothing v p)))
  _ -> error $ "Only base types and lists can be refined: " ++ show (Refinement v t p)

-- | Denotación @[[p]]@ de un predicado sobre la variable @v@. El primer
-- argumento es el tipo base de @v@, o 'Nothing' si @v@ es una lista (cuyo
-- único atributo medible es @len v@).
--
-- Los conectivos se traducen a operaciones de conjuntos (@&&@ ↦ ∩, @||@ ↦ ∪,
-- @not@ ↦ complemento) y cada comparación atómica a un intervalo ('cmpSet').
-- Aquí se verifica también que el predicado esté bien formado: que mencione
-- sólo a @v@, que los literales sean del tipo adecuado, que @len@ se use
-- sólo con strings y listas, etc.
denote :: Maybe Base -> String -> Pred -> ISet
denote mb v = go
  where
    go PTrue = top
    go PFalse = bot
    go (PAnd p q) = inter (go p) (go q)
    go (POr p q) = union (go p) (go q)
    go (PNot p) = complement (go p)
    go (PCmp term op lit) = atom term op lit

    kind = maybe "list" show mb
    isNum = mb `elem` [Just TInt, Just TDouble]
    hasLen = mb == Just TString || mb == Nothing

    atom (TVar x) _ _ | x /= v = unbound x
    atom (TLen x) _ _ | x /= v = unbound x
    -- numéricos: cualquier comparación contra un literal numérico
    atom (TVar _) op (LInt n)    | isNum = cmp op (fromIntegral n)
    atom (TVar _) op (LDouble d) | isNum = cmp op d
    -- booleanos: sólo igualdad o desigualdad con #t / #f
    atom (TVar _) op (LBool x)   | mb == Just TBool && op `elem` [Eq, Neq] = cmp op (bit x)
    -- strings: v == "" y v != "" son azúcar sobre la longitud
    atom (TVar _) Eq  (LStr "")  | mb == Just TString = single 0
    atom (TVar _) Neq (LStr "")  | mb == Just TString = cmpSet CGt 0
    atom (TVar _) _ (LStr _)     | mb == Just TString = error $
      "Refinements over string can only constrain the length (len " ++ v
      ++ ") or compare with the empty string \"\""
    -- longitud de strings y listas
    atom (TLen _) op (LInt n)    | hasLen = cmp op (fromIntegral n)
    atom (TLen _) _ _ | not hasLen = error $ "len " ++ v ++ " is only defined for string and list, not " ++ kind
    atom term op lit = error $ "Ill-formed predicate '" ++ show (PCmp term op lit)
                       ++ "' for a variable of type " ++ kind

    cmp op x = cmpSet (toCmp op) x
    unbound x = error $ "Variable " ++ x ++ " is not the refined variable " ++ v

-- | Puente entre los operadores de la gramática y los del dominio abstracto.
toCmp :: CmpOp -> CmpOp'
toCmp Eq = CEq
toCmp Neq = CNeq
toCmp Lt = CLt
toCmp Le = CLe
toCmp Gt = CGt
toCmp Ge = CGe

-- * Tipado sensible al flujo

-- | @assume g c b@ es el contexto que resulta de suponer que la condición
-- @c@ vale @b@. Sólo se refinan variables (no expresiones compuestas) y
-- siempre de forma __sólida__: el contexto resultante nunca afirma más de lo
-- que la condición garantiza.
--
-- * @not c@ invierte el valor supuesto.
-- * @(&& p q)@ verdadero: valen ambas; falso: vale la negación de alguna,
--   así que se toma la unión ('joinG') de los dos contextos posibles.
-- * @(|| p q)@ es el caso dual.
-- * Una variable booleana sola se fija a #t o #f.
-- * Las comparaciones se delegan a 'assumeCmp', negando el operador cuando se
--   supone falsa.
assume :: Gamma -> ASA -> Bool -> Gamma
assume g (Not c) b = assume g c (not b)
assume g (And p q) True  = assume (assume g p True) q True
assume g (And p q) False = joinG (assume g p False) (assume g q False)
assume g (Or p q) True   = joinG (assume g p True) (assume g q True)
assume g (Or p q) False  = assume (assume g p False) q False
assume g (Id x) b = refineVar g x (single (bit b))
assume g (Cmp op l r) b = assumeCmp g (if b then op else negCmp op) l r
assume g _ _ = g

-- | Refina el contexto suponiendo que @l op r@ es verdadero. Formas
-- reconocidas (las demás dejan el contexto intacto):
--
-- * @x op literal@ y @literal op x@: se intersecta x con @{ v | v op lit }@.
-- * @(len x) op n@: igual, sobre la longitud de la cadena o lista x.
-- * @x op y@ con dos variables del mismo tipo: cada una se acota con los
--   extremos de la otra ('boundBy'). El dominio no es relacional, así que de
--   @x < y@ sólo se aprende @x < sup(y)@ y @y > inf(x)@.
-- * @(len x) op y@ y @y op (len x)@: la longitud de x se acota con y y
--   viceversa.
assumeCmp :: Gamma -> CmpOp -> ASA -> ASA -> Gamma
-- variable contra literal
assumeCmp g op (Id x) (AInt n)    = refineVar g x (cmpSet (toCmp op) (fromIntegral n))
assumeCmp g op (Id x) (ADouble d) = refineVar g x (cmpSet (toCmp op) d)
assumeCmp g op (Id x) (ABool v)   = refineVar g x (cmpSet (toCmp op) (bit v))
assumeCmp g Eq (Id x) (AStr s)    = refineVar g x (single (fromIntegral (length s)))
assumeCmp g Neq (Id x) (AStr "")  = refineVar g x (cmpSet CGt 0)
-- longitud contra literal
assumeCmp g op (Len (Id x)) (AInt n) = refineVar g x (cmpSet (toCmp op) (fromIntegral n))
-- variable contra variable: cada una se acota con los extremos de la otra
assumeCmp g op (Id x) (Id y) = case (measure g x, measure g y) of
  (Just (bx, sx), Just (by, sy)) | bx == by ->
    refineVar (refineVar g x (boundBy op sy)) y (boundBy (flipCmp op) sx)
  _ -> g
-- longitud contra variable entera
assumeCmp g op (Len (Id x)) (Id y) = case (lookupMaybe g x, lookupMaybe g y) of
  (Just tx, Just (R TInt sy)) | Just sx <- lenOf tx ->
    refineVar (refineVar g x (boundBy op sy)) y (boundBy (flipCmp op) sx)
  _ -> g
-- literal o variable contra una forma de las anteriores: se voltea
assumeCmp g op l r@(Id _) | isLit l = assumeCmp g (flipCmp op) r l
assumeCmp g op l r@(Len (Id _)) | isLit l || isVar l = assumeCmp g (flipCmp op) r l
assumeCmp g _ _ _ = g

-- | ¿Es la expresión un literal?
isLit :: ASA -> Bool
isLit (AInt _) = True
isLit (ADouble _) = True
isLit (AStr _) = True
isLit (ABool _) = True
isLit _ = False

isVar :: ASA -> Bool
isVar (Id _) = True
isVar _ = False

-- | Tipo base y conjunto de una variable de tipo base.
measure :: Gamma -> String -> Maybe (Base, ISet)
measure g x = case lookupMaybe g x of
  Just (R b s) -> Just (b, s)
  _ -> Nothing

-- | Conjunto de longitudes de un string o una lista.
lenOf :: RType -> Maybe ISet
lenOf (R TString s) = Just s
lenOf (List _ s) = Just s
lenOf _ = Nothing

-- | @boundBy op s@ = @{ x | existe y ∈ s con x op y }@. Por ejemplo, si
-- @s = [2, 7]@ entonces @boundBy Lt s = (-∞, 7)@.
boundBy :: CmpOp -> ISet -> ISet
boundBy _ [] = bot
boundBy Eq s  = s
boundBy Neq s = maybe top (complement . single) (asSingleton s)
boundBy Lt s  = [mk (-inf) False (supremum s) False]
boundBy Le s  = [mk (-inf) False (supremum s) (hiIn (last s))]
boundBy Gt s  = [mk (infimum s) False inf False]
boundBy Ge s@(iv : _) = [mk (infimum s) (loIn iv) inf False]

-- | Intersecta el refinamiento de la variable visible @x@ con @s@. Sólo se
-- toca la primera aparición (la que está en alcance) y únicamente si tiene
-- una medida (tipo base o lista); registros y funciones no se refinan.
refineVar :: Gamma -> String -> ISet -> Gamma
refineVar [] _ _ = []
refineVar ((y, t) : rest) x s
  | y == x = case t of
      R b s0    -> (y, R b (fitBase b (inter s0 s))) : rest
      List e s0 -> (y, List e (fitLen (inter s0 s))) : rest
      _         -> (y, t) : rest
  | otherwise = (y, t) : refineVar rest x s

-- | Unión punto a punto de dos contextos con la misma forma (ambos provienen
-- de refinar el mismo Γ). Se usa cuando sólo se sabe que vale uno de dos
-- hechos, por ejemplo al suponer falsa una conjunción.
joinG :: Gamma -> Gamma -> Gamma
joinG = zipWith j
  where j (x, R b s1) (_, R _ s2) = (x, R b (fitBase b (union s1 s2)))
        j (x, List e s1) (_, List _ s2) = (x, List e (fitLen (union s1 s2)))
        j p _ = p

-- * Contexto

-- | Tipo de una variable; error si no está en alcance.
lookup :: Gamma -> String -> RType
lookup g x = case lookupMaybe g x of
  Just t  -> t
  Nothing -> error ("Variable " ++ x ++ " not in scope")

lookupMaybe :: [(String, a)] -> String -> Maybe a
lookupMaybe [] _ = Nothing
lookupMaybe ((y, t) : rest) x
  | y == x    = Just t
  | otherwise = lookupMaybe rest x

-- * Impresión de tipos

-- | Representación legible de un tipo normalizado, en la misma sintaxis que
-- usa el programador: @int@, @{ v : int | v >= 1 }@, @int -> boolean@,
-- @{ v : list int | len v >= 1 }@. Un refinamiento igual al tipo completo
-- se imprime sólo como el tipo base.
showR :: RType -> String
showR = showRWith []

-- | Como 'showR', pero imprime con su nombre cualquier subtipo que coincida
-- con un alias declarado (@(type nombre T)@). Así un registro grande se
-- muestra como @tree@ en vez de desplegar todos sus campos.
showRWith :: [(String, RType)] -> RType -> String
showRWith al = go
  where
    go t | Just n <- lookupAlias t = n
    go Bot = "none"
    go (R b s)
      | s == fitBase b top = show b
      | otherwise = "{ v : " ++ show b ++ " | " ++ show (reify (Just b) s) ++ " }"
    go (List e s)
      | s == fitLen top = plain
      | otherwise = "{ v : " ++ plain ++ " | " ++ show (reify Nothing s) ++ " }"
      where plain = "list " ++ showAtom e
    go (Rec fs) = "(record " ++ unwords ["(" ++ f ++ " " ++ go t ++ ")" | (f, t) <- fs] ++ ")"
    go (Fn d c) = showDom d ++ " -> " ++ go c
    showAtom t@(Fn _ _) = "(" ++ go t ++ ")"
    showAtom t@(List _ _) | Nothing <- lookupAlias t = "(" ++ go t ++ ")"
    showAtom t = go t
    showDom t@(Fn _ _) | Nothing <- lookupAlias t = "(" ++ go t ++ ")"
    showDom t = go t
    lookupAlias t = case [n | (n, t') <- al, t' == t] of
      (n : _) -> Just n
      [] -> Nothing

-- | Reconstruye un predicado legible a partir de un conjunto normalizado
-- (inverso aproximado de 'denote'). Cada intervalo se vuelve una conjunción
-- de cotas y la unión de intervalos una disyunción; el complemento de un
-- punto se imprime como @v != k@ y el conjunto vacío como @false@. El
-- primer argumento es el tipo base, o 'Nothing' para una lista.
reify :: Maybe Base -> ISet -> Pred
reify _ [] = PFalse
reify mb s
  | s == maybe (fitLen top) (\b -> fitBase b top) mb = PTrue
  -- v != k : complemento de un punto
  | [Iv l0 _ h hin, Iv l li h1 _] <- s, isInfinite l0, isInfinite h1
  , Just k <- gap h hin l li = PCmp term Neq (lit k)
  | otherwise = foldr1 POr (map ivPred s)
  where
    byLen = mb == Just TString || mb == Nothing
    term = if byLen then TLen "v" else TVar "v"
    isInt = mb /= Just TDouble
    lit x | mb == Just TBool = LBool (x == 1)
          | isInt            = LInt (round x)
          | otherwise        = LDouble x
    -- ¿los dos intervalos dejan fuera exactamente un valor?
    gap h hin l li
      | isInt && not hin && not li = Nothing
      | isInt && l - h == 2 = Just (h + 1)
      | not isInt && h == l && not hin && not li = Just h
      | otherwise = Nothing
    -- una longitud nunca es negativa: se omite len v >= 0
    floorL l li = isInfinite l || (byLen && l == 0 && li)
    ivPred (Iv l li h hin)
      | l == h = PCmp term Eq (lit l)
      | floorL l li && isInfinite h = PTrue
      | floorL l li = upper h hin
      | isInfinite h = lower l li
      | otherwise = PAnd (lower l li) (upper h hin)
    lower l li = PCmp term (if li then Ge else Gt) (lit l)
    upper h hin = PCmp term (if hin then Le else Lt) (lit h)
