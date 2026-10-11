{-|
Module      : Main
Description : REPL de Mini-Cepuac.

Flujo de cada entrada:

>   texto --lexer--> tokens --parseProgram--> items --expand--> ASA
>        --tc--> tipo --interp--> valor

Una entrada (línea del REPL o archivo cargado con @:load@) es una secuencia
de definiciones y expresiones:

* @(type nombre T)@ declara un alias de tipo.
* @(define x : T e)@ y @(define (f x y) : T cuerpo)@ declaran valores y
  funciones recursivas. Se verifican al declararse y quedan disponibles
  para las entradas siguientes.
* Cualquier otra expresión se verifica y evalúa, envuelta en las
  definiciones visibles, y se imprime @valor : tipo refinado inferido@.

El verificador de tipos ('tc') se ejecuta __antes__ que el intérprete y se
fuerza por completo; si rechaza el programa se muestra el error y no se
evalúa nada.

Comandos: @:t <expresión>@ (sólo tipo), @:load <archivo>@ y @(exit)@.
-}
module Main where

import Grammar
import Interp
import Lexer
import Checker
import Desugar
import System.IO (hFlush, stdout, isEOF)
import Control.Exception (catch, SomeException, evaluate, displayException)
import Data.List (sortOn, isPrefixOf)

-- | Estado del REPL: alias de tipos y definiciones acumuladas, de la más
-- antigua a la más reciente.
data Session = Session
  { aliases :: Aliases            -- ^ alias de tipos, sintácticos (para expandir)
  , named   :: [(String, RType)]  -- ^ los mismos alias normalizados (para imprimir)
  , defs    :: [Def]              -- ^ definiciones aceptadas
  }

emptySession :: Session
emptySession = Session [] [] []

-- | Ciclo leer-evaluar-imprimir. Termina con @(exit)@ o fin de entrada.
repl :: Session -> IO ()
repl s = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    if eof then putStrLn "Bye." else do
      input <- getLine
      case input of
        "(exit)" -> putStrLn "Bye."
        "" -> repl s
        (':':'l':'o':'a':'d':' ':filepath) -> load s filepath >>= repl
        (':':'t':' ':expr) -> typeOf s expr >> repl s
        _ -> run s input >>= repl

-- | Procesa un texto completo (definiciones y expresiones) y regresa la
-- sesión extendida con las definiciones que se aceptaron.
run :: Session -> String -> IO Session
run s input = catch
  (do items <- evaluate (parseProgram (lexer input))
      foldlM runItem s items)
  (\e -> handler s e >> return s)

-- | Procesa un elemento del programa.
runItem :: Session -> Item -> IO Session
runItem s item = catch (runItem' s item) (\e -> handler s e >> return s)

runItem' :: Session -> Item -> IO Session
runItem' s (DefType n t) = do
  let t' = expandType (aliases s) t
  rt <- evaluate (normType t')            -- el alias debe estar bien formado
  putStrLn ("type " ++ n ++ " = " ++ showRWith (named s) rt)
  return s { aliases = (n, t') : aliases s, named = (n, rt) : named s }
runItem' s (Expr e) = do
  let ast = wrap (defs s) (expand (aliases s) e)
  t <- checked ast
  putStrLn (show (interp ast) ++ " : " ++ showRWith (named s) t)
  return s
runItem' s item = do
  let d@(x, t, _) = defOf (aliases s) item
      ast = wrap (defs s ++ [d]) (Id x)   -- verifica la definición y sus usos
  _ <- checked ast
  putStrLn (x ++ " : " ++ showRWith (named s) (normType t))  -- el tipo declarado
  return s { defs = defs s ++ [d] }

-- | Verifica tipos forzando el resultado completo, para que cualquier error
-- de tipos aparezca aquí y no durante la evaluación.
checked :: ASA -> IO RType
checked ast = do
  t <- evaluate (tc ([], ast))
  _ <- evaluate (length (showR t))
  return t

-- | Sólo verifica tipos y muestra el tipo inferido de una expresión.
typeOf :: Session -> String -> IO ()
typeOf s input = catch
  (do
    let ast = wrap (defs s) (expand (aliases s) (parse (lexer input)))
    t <- checked ast
    putStrLn (showRWith (named s) t))
  (handler s)

-- | Carga un archivo: definiciones y expresiones, con comentarios @;@.
load :: Session -> String -> IO Session
load s filepath = catch
  (readFile filepath >>= run s)
  (\e -> handler s e >> return s)

-- | Muestra cualquier error (léxico, sintáctico, de tipos o de E/S) sin
-- abortar el REPL. Se conserva sólo la primera línea del mensaje porque GHC
-- agrega la pila de llamadas a los 'error', y los tipos que coinciden con
-- un alias se sustituyen por su nombre para que el mensaje sea legible.
handler :: Session -> SomeException -> IO ()
handler s e = putStrLn ("Error: " ++ useAliases (clean (displayException e)))
  where
    clean = takeWhile (/= '\n')
    -- los alias más largos primero, para que un alias que contiene a otro
    -- se sustituya completo
    useAliases msg = foldl replaceAlias msg (sortOn (negate . length . snd) [(n, showR t) | (n, t) <- named s])
    replaceAlias msg (n, long) = replaceAll long n msg

-- | Sustituye todas las apariciones de una subcadena.
replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll old new str@(c : cs)
  | old `isPrefixOf` str = new ++ replaceAll old new (drop (length old) str)
  | otherwise = c : replaceAll old new cs

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM _ z [] = return z
foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs

banner :: IO ()
banner = do
    putStrLn "=========================================="
    putStrLn "  Cepuac v2.1 - Mini Cepuac Interprete"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn "Comandos disponibles:"
    putStrLn "  - Escribe código directamente"
    putStrLn "  - :t <expresion>   - Muestra el tipo refinado inferido"
    putStrLn "  - :load <archivo>  - Cargar definiciones y expresiones"
    putStrLn "  - (exit)           - Salir"
    putStrLn ""

main :: IO ()
main = banner >> repl emptySession
