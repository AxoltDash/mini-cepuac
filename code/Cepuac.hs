module Main where

import Desugar
import Grammar
import Interp
import Lexer
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)

repl =
  do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
      "(exit)" -> putStrLn "Bye."
      "" -> repl
      (':':'l':'o':'a':'d':' ':filepath) -> do 
        load filepath
        repl
      _ -> do
        eval input
        repl

eval :: String -> IO ()
eval input = catch 
  (putStrLn (show $ interp $ desugar $ parse $ lexer input))
  (\e -> putStrLn $ "Error: " ++ show (e :: SomeException))

load :: String -> IO ()
load filepath = catch
  (do
    src <- readFile filepath
    putStrLn (show $ interp $ desugar $ parse $ lexer src))
  (\e -> putStrLn $ "Error al cargar el archivo " ++ show (e :: SomeException))

run =
  do
    putStrLn "=========================================="
    putStrLn "  Cepuac v1.0 - Mini Lisp Interprete"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn "Comandos disponibles:"
    putStrLn "  - Escribe código directamente"
    putStrLn "  - :load <archivo>  - Cargar desde archivo"
    putStrLn "  - (exit)           - Salir"
    putStrLn ""
    repl

main :: IO ()
main = run
