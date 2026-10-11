{-|
Module      : Refine
Description : Dominio abstracto de los refinamientos: uniones de intervalos.

== Idea

Un predicado de refinamiento sobre una variable @v@ con constantes
(@v > 0@, @v != 3@, @v >= 1 && v <= 10@, ...) denota un subconjunto de la
recta real que siempre se puede escribir como una __unión finita de
intervalos disjuntos__. Este módulo representa los predicados por esa unión
('ISet'), lo que da un procedimiento de decisión completo para la
implicación entre predicados sin necesidad de un solucionador SMT:

>   p ⇒ q      si y sólo si      [[p]] ⊆ [[q]]      ('subset')

Además, la aritmética de intervalos permite __inferir__ el refinamiento del
resultado de una operación a partir del de sus operandos: si @x ∈ [1, 3]@ y
@y ∈ [10, 20]@ entonces @x + y ∈ [11, 23]@.

== Un solo dominio para cuatro tipos

El mismo dominio sirve para todos los tipos base del lenguaje a través de una
"medida" que lleva cada valor a la recta:

>   int     -> el valor (restringido a enteros con 'intNorm')
>   double  -> el valor
>   string  -> su longitud
>   boolean -> 0 (#f) ó 1 (#t)

La restricción a enteros se aplica desde el verificador ("Checker") después
de cada operación; aquí las operaciones trabajan sobre reales.

== Invariante de 'ISet'

Toda función que regresa un 'ISet' lo deja __normalizado__ ('norm'):
intervalos no vacíos, ordenados de izquierda a derecha, disjuntos y sin
contacto entre sí (@[0,1) ∪ [1,2]@ se fusiona en @[0,2]@). Gracias a esto,
un intervalo está contenido en un ISet si y sólo si cabe dentro de alguno de
sus intervalos, lo que hace trivial la inclusión.

== Infinitos

Los extremos se guardan como 'Double' y se usan @±∞@ de IEEE 754 para las
cotas ausentes. Un extremo infinito nunca está incluido ('mk' lo garantiza).
Las operaciones aritméticas cuidan los casos @0 · ∞@ y @1 / 0@.
-}
module Refine where

import Data.List (sortOn)

-- * Intervalos y conjuntos

-- | Intervalo @⟨lo, hi⟩@ con extremos posiblemente infinitos. 'loIn' y 'hiIn'
-- indican si el extremo correspondiente está incluido (cerrado) o no
-- (abierto). Por ejemplo @Iv 0 False 5 True@ es @(0, 5]@.
data Interval = Iv { lo :: Double, loIn :: Bool, hi :: Double, hiIn :: Bool }
  deriving (Eq, Show)

-- | Unión finita de intervalos. Invariante: no vacíos, disjuntos, ordenados
-- y sin contacto (ver 'norm').
type ISet = [Interval]

-- | Infinito positivo; @-inf@ es el negativo.
inf :: Double
inf = 1 / 0

-- | Constructor inteligente: un extremo infinito nunca está incluido.
mk :: Double -> Bool -> Double -> Bool -> Interval
mk l li h hin = Iv l (li && not (isInfinite l)) h (hin && not (isInfinite h))

-- | Toda la recta: @(-∞, ∞)@. Denotación del predicado @true@.
top :: ISet
top = [mk (-inf) False inf False]

-- | Conjunto vacío. Denotación del predicado @false@ (código inalcanzable).
bot :: ISet
bot = []

-- | El conjunto @{x}@.
single :: Double -> ISet
single x = [mk x True x True]

-- | ¿Es el intervalo no vacío? Un intervalo degenerado @[x, x]@ sólo es
-- válido si ambos extremos están incluidos y x es finito.
valid :: Interval -> Bool
valid (Iv l li h hin) = l < h || (l == h && li && hin && not (isInfinite l))

-- | Normaliza una lista arbitraria de intervalos al invariante de 'ISet':
-- descarta los vacíos, ordena por extremo izquierdo (un extremo cerrado va
-- antes que el mismo extremo abierto) y fusiona los que se traslapan o se
-- tocan.
norm :: [Interval] -> ISet
norm ivs = merge (sortOn key (filter valid ivs))
  where
    key (Iv l li _ _) = (l, not li)
    merge (a:b:rest)
      | touches a b = merge (hull a b : rest)
      | otherwise   = a : merge (b : rest)
    merge xs = xs
    -- a y b se traslapan, o terminan/empiezan en el mismo punto y al menos
    -- uno de los dos lo incluye
    touches a b = hi a > lo b || (hi a == lo b && (hiIn a || loIn b))
    hull a b = let (h, hin) = maxHi a b in mk (lo a) (loIn a) h hin

-- | Extremo derecho más grande de dos intervalos (con su inclusión).
maxHi :: Interval -> Interval -> (Double, Bool)
maxHi a b
  | hi a > hi b = (hi a, hiIn a)
  | hi a < hi b = (hi b, hiIn b)
  | otherwise   = (hi a, hiIn a || hiIn b)

-- | Extremo derecho más chico de dos intervalos (para intersección).
minHi :: Interval -> Interval -> (Double, Bool)
minHi a b
  | hi a < hi b = (hi a, hiIn a)
  | hi a > hi b = (hi b, hiIn b)
  | otherwise   = (hi a, hiIn a && hiIn b)

-- | Extremo izquierdo más grande de dos intervalos (para intersección).
maxLo :: Interval -> Interval -> (Double, Bool)
maxLo a b
  | lo a > lo b = (lo a, loIn a)
  | lo a < lo b = (lo b, loIn b)
  | otherwise   = (lo a, loIn a && loIn b)

-- * Operaciones de conjuntos

-- | Intersección de dos intervalos (puede resultar vacío; 'norm' lo filtra).
interIv :: Interval -> Interval -> Interval
interIv a b = let (l, li) = maxLo a b
                  (h, hin) = minHi a b
              in mk l li h hin

-- | Intersección @A ∩ B@. Denotación de @&&@.
inter :: ISet -> ISet -> ISet
inter xs ys = norm [interIv x y | x <- xs, y <- ys]

-- | Unión @A ∪ B@. Denotación de @||@ y de la unión de ramas de un @if@.
union :: ISet -> ISet -> ISet
union xs ys = norm (xs ++ ys)

-- | Complemento @ℝ \\ A@. Denotación de @not@. Recorre los intervalos de
-- izquierda a derecha emitiendo los huecos entre ellos, con la inclusión de
-- cada extremo invertida.
complement :: ISet -> ISet
complement ivs = norm (go (-inf) True ivs)
  where
    go prevHi prevIn [] = [mk prevHi (not prevIn) inf False]
    go prevHi prevIn (Iv l li h hin : rest) =
      mk prevHi (not prevIn) l (not li) : go h hin rest

-- | @contains big small@: ¿está @small@ dentro de @big@?
contains :: Interval -> Interval -> Bool
contains big small =
  (lo big < lo small || (lo big == lo small && (loIn big || not (loIn small)))) &&
  (hi big > hi small || (hi big == hi small && (hiIn big || not (hiIn small))))

-- | Inclusión @A ⊆ B@, es decir, la implicación @p ⇒ q@ entre predicados.
-- Como B está normalizado (sin intervalos en contacto), un intervalo de A
-- cabe en B si y sólo si cabe en alguno de sus intervalos.
subset :: ISet -> ISet -> Bool
subset xs ys = all (\x -> any (`contains` x) ys) xs

-- | Pertenencia @x ∈ A@.
member :: Double -> ISet -> Bool
member x = any (\iv -> contains iv (mk x True x True))

isEmpty :: ISet -> Bool
isEmpty = null

-- | Si el conjunto es un único punto, lo regresa.
asSingleton :: ISet -> Maybe Double
asSingleton [Iv l True h True] | l == h = Just l
asSingleton _ = Nothing

-- | Ínfimo del conjunto (@+∞@ si es vacío).
infimum :: ISet -> Double
infimum [] = inf
infimum (iv : _) = lo iv

-- | Supremo del conjunto (@-∞@ si es vacío).
supremum :: ISet -> Double
supremum [] = -inf
supremum ivs = hi (last ivs)

-- | Operadores de comparación del dominio. Copia local de los de la
-- gramática para que este módulo no dependa de ella.
data CmpOp' = CEq | CNeq | CLt | CLe | CGt | CGe
  deriving (Eq, Show)

-- | Denotación de una comparación atómica con una constante:
-- @cmpSet op x = { v | v op x }@. Es la base de la traducción de predicados.
cmpSet :: CmpOp' -> Double -> ISet
cmpSet CEq x  = single x
cmpSet CNeq x = complement (single x)
cmpSet CLt x  = [mk (-inf) False x False]
cmpSet CLe x  = [mk (-inf) False x True]
cmpSet CGt x  = [mk x False inf False]
cmpSet CGe x  = [mk x True inf False]

-- * Restricción a enteros

-- | Reduce cada intervalo al mayor intervalo cerrado de enteros que contiene
-- (@(0, 2.5)@ pasa a @[1, 2]@) y fusiona los intervalos adyacentes
-- (@[1,2] ∪ [3,5] = [1,5]@ sobre enteros). Después de esto, todo intervalo
-- de un conjunto entero es cerrado y tiene extremos enteros, lo que hace que
-- la inclusión entre conjuntos enteros sea exacta.
intNorm :: ISet -> ISet
intNorm ivs = mergeAdj (norm (map intIv ivs))
  where
    intIv (Iv l li h hin) = mk (floorB l li) True (ceilB h hin) True
    -- primer entero dentro del intervalo por la izquierda
    floorB l li
      | isInfinite l = l
      | li           = fromIntegral (ceiling l :: Integer)
      | otherwise    = fromIntegral (floor l :: Integer) + 1
    -- último entero dentro del intervalo por la derecha
    ceilB h hin
      | isInfinite h = h
      | hin          = fromIntegral (floor h :: Integer)
      | otherwise    = fromIntegral (ceiling h :: Integer) - 1
    mergeAdj (a:b:rest)
      | hi a + 1 >= lo b = mergeAdj (mk (lo a) True (max (hi a) (hi b)) True : rest)
      | otherwise        = a : mergeAdj (b : rest)
    mergeAdj xs = xs

-- * Aritmética de intervalos

-- | @[a, b] + [c, d] = [a + c, b + d]@; un extremo del resultado está
-- incluido sólo si lo estaban los dos sumandos.
addIv :: Interval -> Interval -> Interval
addIv a b = mk (lo a + lo b) (loIn a && loIn b) (hi a + hi b) (hiIn a && hiIn b)

-- | @-[a, b] = [-b, -a]@.
negIv :: Interval -> Interval
negIv (Iv l li h hin) = mk (negate h) hin (negate l) li

-- | Suma, resta y producto de conjuntos: se operan todos los pares de
-- intervalos y se normaliza el resultado.
addSet, subSet, mulSet :: ISet -> ISet -> ISet
addSet xs ys = norm [addIv x y | x <- xs, y <- ys]
subSet xs ys = norm [addIv x (negIv y) | x <- xs, y <- ys]
mulSet xs ys = norm [mulIv x y | x <- xs, y <- ys]

-- | Producto de dos intervalos. Como el producto es continuo y la imagen de
-- un intervalo es un intervalo, basta tomar la envolvente de los cuatro
-- productos de extremos. Un extremo del resultado se alcanza si ambos
-- factores se alcanzan, o si alguno de ellos es un cero alcanzable
-- (@0 · y = 0@ para cualquier y, incluso con el otro extremo infinito).
mulIv :: Interval -> Interval -> Interval
mulIv a b = mk l li h hin
  where
    ends iv = [(lo iv, loIn iv), (hi iv, hiIn iv)]
    corners = [ (mulB x y, (xi && yi) || (xi && x == 0) || (yi && y == 0))
              | (x, xi) <- ends a, (y, yi) <- ends b ]
    l   = minimum (map fst corners)
    h   = maximum (map fst corners)
    li  = or [i | (v, i) <- corners, v == l]
    hin = or [i | (v, i) <- corners, v == h]
    -- evita 0 · ∞ = NaN
    mulB x y | x == 0 || y == 0 = 0
             | otherwise        = x * y

-- | Recíproco @1 / [a, b]@ de un intervalo que __no contiene al cero__ (por
-- lo tanto a y b tienen el mismo signo). Los extremos se invierten y se
-- intercambian: @1 / [2, ∞) = (0, 1/2]@ y @1 / [-3, 0) = (-∞, -1/3]@.
recipIv :: Interval -> Interval
recipIv (Iv l li h hin) = mk l' hin h' li
  where
    l' | h == 0        = -inf
       | isInfinite h  = 0
       | otherwise     = 1 / h
    h' | l == 0        = inf
       | isInfinite l  = 0
       | otherwise     = 1 / l

-- | División real @A / B = A · (1 / B)@. Precondición: @0 ∉ B@, garantizada
-- por el verificador antes de llamar.
divSet :: ISet -> ISet -> ISet
divSet xs ys = norm [mulIv x (recipIv y) | x <- xs, y <- ys]

-- | Truncamiento hacia cero, usado por @toInt@ y la división entera. Como
-- @truncate@ es monótona basta truncar los extremos.
truncSet :: ISet -> ISet
truncSet = intNorm . map truncIv
  where
    truncIv (Iv l _ h _) = mk (tr l) True (tr h) True
    tr x | isInfinite x = x
         | otherwise    = fromIntegral (truncate x :: Integer)

-- | División entera (@quot@): truncamiento del cociente real.
quotSet :: ISet -> ISet -> ISet
quotSet xs ys = truncSet (divSet xs ys)

-- | Residuo entero (@rem@): tiene el signo del dividendo y
-- @|r| < max |divisor|@. Si ambos operandos son constantes el resultado es
-- exacto. Precondición: conjuntos enteros normalizados y @0 ∉ ys@.
remSet :: ISet -> ISet -> ISet
remSet xs ys
  | isEmpty xs || isEmpty ys = bot
  | Just x <- asSingleton xs, Just y <- asSingleton ys =
      single (fromIntegral (truncate x `rem` (truncate y :: Integer)))
  | otherwise = intNorm (map remIv xs)
  where
    bound = maximum [max (abs (lo y)) (abs (hi y)) | y <- ys] - 1
    remIv (Iv l _ h _)
      | l >= 0    = mk 0 True (min h bound) True
      | h <= 0    = mk (max l (negate bound)) True 0 True
      | otherwise = mk (max l (negate bound)) True (min h bound) True
