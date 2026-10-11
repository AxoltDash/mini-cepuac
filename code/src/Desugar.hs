{-|
Module      : Desugar
Description : Expansión de alias de tipos y de azúcar sintáctica.

Entre el parser y el verificador hay una pasada puramente sintáctica que:

* sustituye los alias declarados con @(type nombre T)@ ('TName') por su
  definición en todas las anotaciones de tipo;
* traduce las lambdas de varios parámetros ('LambdaN') a lambdas anidadas
  de un parámetro, repartiendo el tipo flecha entre ellas;
* convierte una definición de nivel superior en un 'Letrec'.

Después de 'expand' el ASA ya no contiene 'LambdaN' ni 'TName', y el
verificador no necesita saber que existieron.
-}
module Desugar where

import Grammar

-- | Tabla de alias: nombre ↦ tipo ya expandido.
type Aliases = [(String, Type)]

-- | Sustituye los alias dentro de un tipo. Un alias desconocido es un error.
expandType :: Aliases -> Type -> Type
expandType al = go
  where
    go (TName n) = case lookup n al of
      Just t  -> t
      Nothing -> error ("Unknown type alias: " ++ n)
    go (TBase b) = TBase b
    go (TList t) = TList (go t)
    go (TRecord fs) = TRecord [(f, go t) | (f, t) <- fs]
    go (Arrow d c) = Arrow (go d) (go c)
    go (Refinement v t p) = Refinement v (go t) p

-- | Expande alias y azúcar en una expresión.
expand :: Aliases -> ASA -> ASA
expand al = go
  where
    ty = expandType al
    go e = case e of
      AInt _ -> e
      ADouble _ -> e
      AStr _ -> e
      ABool _ -> e
      Id _ -> e
      Add a b -> Add (go a) (go b)
      Sub a b -> Sub (go a) (go b)
      Mul a b -> Mul (go a) (go b)
      Div a b -> Div (go a) (go b)
      Mod a b -> Mod (go a) (go b)
      Concat a b -> Concat (go a) (go b)
      Len a -> Len (go a)
      ListLit es -> ListLit (map go es)
      Cons a b -> Cons (go a) (go b)
      Head a -> Head (go a)
      Tail a -> Tail (go a)
      Nth a i -> Nth (go a) (go i)
      SetNth a i v -> SetNth (go a) (go i) (go v)
      Replicate n v -> Replicate (go n) (go v)
      Record fs -> Record [(f, go v) | (f, v) <- fs]
      Get r f -> Get (go r) f
      With r f v -> With (go r) f (go v)
      ToInt a -> ToInt (go a)
      ToDouble a -> ToDouble (go a)
      And a b -> And (go a) (go b)
      Or a b -> Or (go a) (go b)
      Not a -> Not (go a)
      Cmp op a b -> Cmp op (go a) (go b)
      If c t f -> If (go c) (go t) (go f)
      LambdaN t xs b -> mkLambda (ty t) xs (go b)
      Lambda t x b -> Lambda (ty t) x (go b)
      App f a -> App (go f) (go a)
      Let (x, t) v b -> Let (x, fmap ty t) (go v) (go b)
      Letrec (x, t) v b -> Letrec (x, ty t) (go v) (go b)

-- | Reparte un tipo flecha entre varios parámetros:
--
-- > mkLambda (A -> B -> C) [x, y] b  =  Lambda (A -> B -> C) x (Lambda (B -> C) y b)
mkLambda :: Type -> [String] -> ASA -> ASA
mkLambda t [x] b = Lambda t x b
mkLambda t@(Arrow _ rest) (x : xs) b = Lambda t x (mkLambda rest xs b)
mkLambda t xs _ = error $ "lambda with parameters (" ++ unwords xs
                  ++ ") needs a function type with that many arrows, got " ++ show t

-- | Una definición de nivel superior como ligadura recursiva:
-- nombre, tipo anotado y valor, ya expandidos.
type Def = (String, Type, ASA)

-- | Traduce un 'DefVal' o 'DefFun' a una ligadura recursiva.
defOf :: Aliases -> Item -> Def
defOf al (DefVal x t v) = (x, expandType al t, expand al v)
defOf al (DefFun f t xs b) = let t' = expandType al t
                             in (f, t', mkLambda t' xs (expand al b))
defOf _ item = error ("Not a definition: " ++ show item)

-- | Envuelve una expresión con las definiciones visibles, de la más antigua
-- (más externa) a la más reciente. Cada definición ve a las anteriores y a
-- sí misma.
wrap :: [Def] -> ASA -> ASA
wrap defs body = foldr (\(x, t, v) b -> Letrec (x, t) v b) body defs
