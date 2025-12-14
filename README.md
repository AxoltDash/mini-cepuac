# Mini-Cepuac Interprete

Un intérprete de un lenguaje funcional tipo Lisp con semántica operacional de paso grande, implementado en Haskell.

## 📋 Tabla de Contenidos

- [Requisitos Previos](#requisitos-previos)
- [Instalación](#instalación)
- [Ejecución](#ejecución)
- [Uso del Intérprete Interactivo](#uso-del-intérprete-interactivo)
- [Sintaxis del Lenguaje](#sintaxis-del-lenguaje)


## 🔧 Requisitos Previos

Necesitas tener instalado:

- **GHCup** 
- **Happy** 


## Instalación de GHCup
En Linux (incluyendo WSL2) y macOS. Abra una terminal y ejecute el siguiente comando:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Siga las instrucciones en pantalla. GHCup descargará e instalará automáticamente *GHC*, *GHCi* y *Cabal*, configurando el entorno de forma completa.

En Windows
1. Descargue el instalador de *GHCup* desde el sitio oficial de Haskell. @haskell
2. Ejecute el instalador y siga las instrucciones del asistente.

## Instalación de Happy

_Happy_ es un generador de analizadores sintácticos para Haskell. Se distribuye como un paquete de Cabal, por lo que su instalación es directa. @happy

1. Abra la terminal.
2. Actualice el índice de paquetes de Cabal:
3. Instale _Happy_:

```bash
cabal update
cabal install happy
```


### Verificar instalación:

```bash
ghci --version
happy --version
```

## 📦 Instalación de Cepuac

1. **Clonar el repositorio:**

```bash
git clone https://github.com/AxoltDash/mini-lisp.git](https://github.com/AxoltDash/mini-cepuac.git
cd mini-cepuac
```

2. **Estructura del proyecto:**

```
mini-cepuac/
├── code/
│   ├── Cepuac.hs          # Punto de entrada (REPL)
│   ├── ej/          # Ejemplos de programas
│   └── src/
│       ├── Lexer.hs       # Analizador léxico
│       ├── Grammar.y      # Gramática (Happy)
│       ├── Grammar.hs     # Parser generado
│       ├── Checker.hs     # Verificacion de tipo
│       └── Interp.hs      # Intérprete (semántica)
├── docs/                  # Investigación proyecto02
└── README.md
```


## 🚀 Ejecución

### Modo Interactivo

A la altura de code/
```bash
ghci -isrc -package array Cepuac.hs
```

Verás el siguiente mensaje de bienvenida:

```
==========================================
  Caupec v1.0 - Mini Cepuac Interprete
==========================================

Comandos disponibles:
  - Escribe código directamente
  - :load <archivo>  - Cargar desde archivo
  - (exit)           - Salir
> 
```

### Cargar y ejecutar un archivo

```bash
> :load ej/safediv.cpc

```

## 💻 Uso del Intérprete Interactivo

### Comandos del 

| Comando | Descripción |
|---------|-------------|
| `<expresión>` | Evalúa la expresión directamente |
| `:load <archivo>` | Carga y ejecuta un archivo |
| `(exit)` | Sale del intérprete |

### Ejemplos de uso en el modo interactivo

```lisp
> (+ 3 5)
 8.0

> (* 2 3 4)
 24.0

> ((lambda : boolean -> boolean (b) b) #t)
#t

> (exit)
Bye.
```

## Sintaxis del Lenguaje

### Operadores Aritméticos

```lisp
(+ 1 2 3 4)           ; Suma: 10
(- 10 3)              ; Resta: 7
(* 2 3 4)             ; Multiplicación: 24
(/ 20 4)              ; División: 5
```

### Operadores Lógicos y de Comparación

```lisp
(&& #t #t #f)         ; AND: #f
(|| #f #f #t)         ; OR: #t
(not #t)              ; NOT: #f
```

### Funciones Lambda

```lisp
; Lambda simple
((lambda : number -> number (b) (+ 1 b)) 3)  ; Resultado: 4

```

### Enlaces Locales

```lisp
; Let
 (let (x :number 2) (* x x))                 ; Resultado: 4

```

**¡Disfruta programando en Cepuac!**
