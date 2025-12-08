module Main where

import Grammar
import Interp
import Lexer
import Checker
import System.IO (hFlush, stdout)
import Control.Exception (catch, ErrorCall(..), evaluate)

repl = do
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
  (do
    let ast = parse $ lexer input
    t <- evaluate $ tc ([], ast)  -- Verificación de tipos (forzada)
    putStrLn (show $ interp ast t))
  (\(ErrorCall msg) -> putStrLn $ "Error: " ++ msg)

load :: String -> IO ()
load filepath = catch
  (do
    src <- readFile filepath
    let ast = parse $ lexer src
    t <- evaluate $ tc ([], ast)  -- Verificación de tipos (forzada)
    putStrLn (show $ interp ast t))
  (\(ErrorCall msg) -> putStrLn $ "Error: " ++ msg)


run =
  do
    putStrLn "=========================================="
    putStrLn "  Caupec v1.0 - Mini Cepuac Interprete"
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


