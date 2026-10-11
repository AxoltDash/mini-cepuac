# Mini-Cepuac Interprete

Un intérprete de un lenguaje funcional tipo Lisp con semántica operacional de paso grande y un sistema de **tipos refinados** nativo, implementado en Haskell.

El verificador de tipos infiere, para cada expresión, el conjunto de valores que puede tomar y lo usa para rechazar estáticamente programas inseguros: divisiones entre cero, conversiones fuera de rango, cadenas vacías donde se exige contenido, etc.

## 📋 Tabla de Contenidos

- [Requisitos Previos](#-requisitos-previos)
- [Instalación](#-instalación-de-cepuac)
- [Ejecución](#-ejecución)
- [Uso del Intérprete Interactivo](#-uso-del-intérprete-interactivo)
- [Sintaxis del Lenguaje](#sintaxis-del-lenguaje)
- [Sistema de Tipos Refinados](#sistema-de-tipos-refinados)
- [Ejemplos](#ejemplos)


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
git clone https://github.com/AxoltDash/mini-cepuac.git
cd mini-cepuac
```

2. **Estructura del proyecto:**

```
mini-cepuac/
├── code/
│   ├── Cepuac.hs          # Punto de entrada (REPL)
│   ├── run_examples.sh    # Ejecuta todos los programas de ej/
│   ├── ej/                # Ejemplos (los failed_* deben fallar); incluye multiset,
│   │                      #   fibheap y linkcut, tres estructuras de datos completas
│   └── src/
│       ├── Lexer.hs       # Analizador léxico
│       ├── Grammar.y      # Gramática (Happy)
│       ├── Grammar.hs     # Parser generado
│       ├── Desugar.hs     # Alias de tipos y azúcar sintáctica
│       ├── Refine.hs      # Dominio abstracto de los refinamientos (intervalos)
│       ├── Checker.hs     # Verificador de tipos refinados
│       └── Interp.hs      # Intérprete (semántica)
├── docs/                  # Investigación proyecto02
└── README.md
```

3. **Regenerar el parser** (sólo si se modifica `Grammar.y`):

```bash
cd code
happy --ghc -o src/Grammar.hs src/Grammar.y
```


## 🚀 Ejecución

### Modo Interactivo

A la altura de code/
```bash
ghci -isrc -package array Cepuac.hs
```

y dentro de GHCi escribe `main`. Verás el siguiente mensaje de bienvenida:

```
==========================================
  Cepuac v2.1 - Mini Cepuac Interprete
==========================================

Comandos disponibles:
  - Escribe código directamente
  - :t <expresion>   - Muestra el tipo refinado inferido
  - :load <archivo>  - Cargar definiciones y expresiones
  - (exit)           - Salir
>
```

### Cargar y ejecutar un archivo

```bash
> :load ej/safediv.cpc
5 : int
```

### Ejecutar todos los ejemplos

```bash
./run_examples.sh
```

## 💻 Uso del Intérprete Interactivo

### Comandos del REPL

| Comando | Descripción |
|---------|-------------|
| `<expresión>` | Verifica, evalúa e imprime `valor : tipo refinado` |
| `:t <expresión>` | Sólo verifica y muestra el tipo refinado inferido |
| `(define ...)`, `(type ...)` | Declara una función, valor o alias de tipo que persiste en la sesión |
| `:load <archivo>` | Carga un archivo: sus definiciones quedan disponibles en el REPL |
| `(exit)` | Sale del intérprete |

### Ejemplos de uso en el modo interactivo

```lisp
> (+ 3 5)
8 : { v : int | v == 8 }

> (/ 10.0 4.0)
2.5 : { v : double | v == 2.5 }

> (++ "hola" " mundo")
"hola mundo" : { v : string | len v == 10 }

> ((lambda : boolean -> boolean (b) b) #t)
#t : boolean

> (/ 1 (- 3 3))
Error: Possible division by zero: the divisor of / has type { v : int | v == 0 }, which admits 0

> :t (lambda : { v : int | v != 0 } -> int (y) (/ 10 y))
{ v : int | v != 0 } -> int

> (define (fact n) : int -> int (if (<= n 1) 1 (* n (fact (- n 1)))))
fact : int -> int

> (fact 10)
3628800 : int

> :load ej/multiset.cpc
...
> (ms-to-list (ms-insert (ms-insert ms-new 7) 3))
[3, 7] : list int

> (exit)
Bye.
```

## Sintaxis del Lenguaje

### Tipos base

| Tipo | Literales | Notas |
|------|-----------|-------|
| `int` | `0`, `42`, `-7` | Enteros de precisión arbitraria. `/` es división entera, `%` residuo |
| `double` | `3.0`, `2.5`, `-0.5` | Punto flotante. Un literal es `double` sólo si lleva punto decimal |
| `string` | `"hola"`, `"con \"escapes\"\n"` | Escapes `\" \\ \n \t` |
| `boolean` | `#t`, `#f` | |

| `list T` | `(list 1 2 3)`, `(list)` | Listas inmutables homogéneas |
| `(record (f T) ...)` | `(record (x 1) (y 2))` | Registros inmutables con campos nombrados |

`int` y `double` son tipos distintos: `(+ 1 2.5)` es un error de tipos. La conversión es explícita con `toDouble` y `toInt`.

Los identificadores pueden llevar `-`, `_`, `?` y `'` (`red?`, `ms-insert`), así que los operadores siempre van separados por espacios: `(- x 1)`, `int -> int`.

### Operadores Aritméticos

Ambos operandos deben ser `int` o ambos `double`.

```lisp
(+ 1 2)           ; Suma: 3
(- 10 3)          ; Resta: 7
(* 2 3)           ; Multiplicación: 6
(/ 20 4)          ; División entera: 5      (/ 7 2) = 3
(/ 20.0 8.0)      ; División real: 2.5
(% 10 3)          ; Residuo (sólo int): 1
(toDouble 3)      ; 3.0
(toInt 3.7)       ; 3 (trunca hacia cero)
```

### Cadenas

```lisp
(++ "ho" "la")    ; Concatenación: "hola"
(len "hola")      ; Longitud: 4
(== "a" "b")      ; Igualdad: #f
```

### Listas

```lisp
(list 1 2 3)          ; [1, 2, 3]       (list) es la lista vacía
(cons 0 xs)           ; agrega al frente
(head xs) (tail xs)   ; exigen probar que len xs > 0
(nth xs i)            ; exige probar que 0 <= i < len xs
(set xs i v)          ; copia de xs con la posición i cambiada (mismo requisito)
(replicate n v)       ; n copias de v (n >= 0)
(len xs) (++ xs ys)   ; longitud y concatenación, como en cadenas
```

El refinamiento de una lista habla de su longitud: `{ v : list int | len v > 0 }`. Los literales tienen longitud exacta, `cons` suma 1, `tail` resta 1 y `++` suma. Con `if` sobre `(len xs)` el verificador refina la lista dentro de la rama:

```lisp
(lambda : list int -> int (xs) (if (== (len xs) 0) 0 (head xs)))   ; OK
(lambda : list int -> int (xs) (head xs))                           ; Error: admite length 0
```

### Registros

```lisp
(record (x 1) (y "a"))        ; tipo (record (x int) (y string))
(get r x)                     ; lee un campo
(with r x 5)                  ; copia de r con el campo x cambiado
```

Los campos pueden llevar refinamientos: `(record (balance { v : int | v >= 0 }))` obliga a que todo `with` sobre `balance` reciba un natural.

### Comparaciones

Regresan `boolean`. Ambos operandos deben ser del mismo tipo; `string` y `boolean` sólo admiten `==` y `!=`.

```lisp
(== 1 1)  (!= 1 2)  (< 1 2)  (<= 1 1)  (> 2 1)  (>= 2 2)
```

### Operadores Lógicos

```lisp
(&& #t #t)         ; AND: #t
(|| #f #f)         ; OR: #f
(not #t)           ; NOT: #f
```

### Condicional

```lisp
(if (< 1 2) "si" "no")      ; "si"
```

Ambas ramas deben tener el mismo tipo base; el tipo del `if` es la unión de los refinamientos de las ramas.

### Funciones Lambda

```lisp
((lambda : int -> int (b) (+ 1 b)) 3)             ; Resultado: 4
((lambda : string -> int (s) (len s)) "hola")      ; Resultado: 4
```

La anotación es el tipo completo de la función. El cuerpo se verifica al definir la lambda, suponiendo que el parámetro tiene el tipo del dominio, y debe ser subtipo del codominio.

Una lambda puede tener varios parámetros; es azúcar sobre lambdas anidadas y la aplicación acepta varios argumentos:

```lisp
((lambda : int -> int -> int (a b) (+ a b)) 3 4)      ; 7
```

### Enlaces Locales

```lisp
(let (x : int 2) (* x x))                              ; Resultado: 4
(let (x : { v : int | v > 0 } 2) (/ 10 x))             ; Resultado: 5
```

El valor debe ser subtipo del tipo anotado. Dentro del cuerpo, la variable tiene el tipo **más preciso** que el verificador infirió para el valor (en el ejemplo, `{ v : int | v == 2 }`).

Con varias ligaduras la anotación es opcional y cada una ve a las anteriores, lo que permite escribir código "imperativo" rebindeando el mismo nombre:

```lisp
(let ((t (set-left t h x))
      (t (set-red t x #t))
      (n : int (+ 1 2)))
  (with t ret n))
```

### Recursión y definiciones

`letrec` liga un nombre visible dentro de su propio valor (con el tipo anotado):

```lisp
(letrec (fact : int -> int (lambda : int -> int (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
  (fact 5))
```

En archivos y en el REPL se pueden escribir definiciones de nivel superior, que son recursivas y persisten para las entradas siguientes. Cada definición sólo ve a las anteriores (no hay recursión mutua):

```lisp
(type nat { v : int | v >= 0 })                 ; alias de tipo
(define limite : nat 64)                         ; valor
(define (fact n) : nat -> nat                    ; función (el tipo es el de la función completa)
  (if (<= n 1) 1 (* n (fact (- n 1)))))
(fact 5)                                         ; expresión: se evalúa e imprime
```

La terminación no se verifica.

## Sistema de Tipos Refinados

Un tipo refinado tiene la forma `{ v : T | p }`, donde `T` es un tipo base (o a su vez un tipo refinado) y `p` es un predicado sobre `v`:

```
Pred ::= Term op Lit | Lit op Term
       | Pred && Pred | Pred || Pred | not Pred | ( Pred )
Term ::= v | len v                 (len v sólo para string)
op   ::= == | != | < | <= | > | >=
Lit  ::= int | double | string | #t | #f
```

Ejemplos de tipos:

```lisp
{ v : int | v != 0 }
{ v : int | v >= 1 && v <= 10 }
{ v : int | v < 0 || v > 100 }
{ v : double | v > 0.0 }
{ v : string | len v > 0 }
{ v : string | v != "" }               ; equivalente al anterior
{ v : list int | len v == 64 }         ; listas: también por longitud
{ v : boolean | v == #t }
{ v : { w : int | w > 0 } | v < 10 }   ; anidado: intersección de ambos
int -> { v : int | v != 0 } -> int     ; refinamientos en funciones
```

Sobre `string` y `list` los refinamientos hablan de la **longitud** (`len v`); en cadenas también se admite la igualdad con `""`. Los registros y las funciones no se refinan directamente, pero sus componentes sí.

### Cómo funciona

- **Denotación.** Cada predicado denota una unión finita de intervalos de la recta real (`Refine.hs`). La implicación entre predicados, `p ⇒ q`, se decide como inclusión de conjuntos, sin necesidad de un solucionador SMT. Para `int` los intervalos se ajustan a enteros, para `string` el conjunto describe la longitud y para `boolean` los valores `0`/`1`.
- **Inferencia.** El verificador calcula el refinamiento más preciso de cada expresión: los literales son singuletes, la aritmética usa aritmética de intervalos (`(* y 2)` con `y ∈ [1,10]` da `[2,20]`), `++` suma longitudes, `toInt` trunca, y las comparaciones cuyo resultado ya está decidido producen un `boolean` singulete.
- **Subtipado.** `{ v : T | p } <: { v : T | q }` sii `[[p]] ⊆ [[q]]`. Los tipos función son contravariantes en el dominio y covariantes en el codominio.
- **Tipado sensible al flujo.** En `(if c t e)`, dentro de `t` se asume `c` y dentro de `e` se asume `not c`, refinando las variables que aparecen en la condición (incluyendo `&&`, `||`, `not`, `len x` y comparaciones entre dos variables):

  ```lisp
  (lambda : int -> int (y) (if (!= y 0) (/ 10 y) 0))      ; OK: en la rama then, y != 0
  (lambda : int -> int (y) (if (> y 0) (/ 10 y) (/ 10 y))) ; Error: en la rama else, y <= 0 admite 0
  ```
- **Seguridad.** `/` y `%` exigen que el refinamiento del divisor excluya al `0`; `head` y `tail` que la lista no pueda ser vacía; `nth` y `set` que el índice quepa en todas las longitudes posibles. Si el verificador no puede probarlo, el programa se rechaza antes de ejecutarse.

Los tipos se imprimen en forma canónica: `{ v : int | v > 0 }` se muestra como `{ v : int | v >= 1 }`, y `{ v : int | v < 0 || v > 0 }` como `{ v : int | v != 0 }`.

### Limitaciones

El dominio es **no relacional**: cada variable se analiza por separado, así que hechos como `x < y ⇒ y - x > 0`, `b || not b` o `i < (len xs) ⇒ (nth xs i)` seguro no se pueden demostrar. En esos casos se liga la subexpresión con `let` y se refina con `if` (por ejemplo `(let ((d (- a b))) (if (>= d 0) d 0))`), o se fijan longitudes e índices con tipos concretos (`len v == 64`, `v < 64`).

## Ejemplos

Todos están en `code/ej/`. Los archivos `failed_*` e `invalid_*` deben producir un error de tipos.

| Archivo | Qué muestra |
|---------|-------------|
| `safediv.cpc` / `failed_safediv.cpc` | División segura exigiendo `{ y : int \| y != 0 }` |
| `unsafe_lambda.cpc` | El cuerpo de una lambda se verifica al definirla |
| `if_refine.cpc` / `failed_if_refine.cpc` | Tipado sensible al flujo con `if` |
| `intervals.cpc` / `failed_intervals.cpc` | Aritmética de intervalos sobre el codominio |
| `doubles.cpc` / `failed_doubles.cpc` / `failed_mixed.cpc` | `int` vs `double`, `toInt`, `toDouble` |
| `ariane.cpc` / `failed_ariane.cpc` | Conversión a 16 bits con rango verificado (el fallo del Ariane 5) |
| `strings.cpc` / `failed_strings.cpc` / `strings_if.cpc` | Refinamientos sobre la longitud de cadenas |
| `modulo.cpc` | Residuo acotado por el refinamiento del divisor |
| `lists.cpc` / `failed_lists.cpc` / `failed_index.cpc` | `head` y `nth` seguros por refinamiento de longitud e índice |
| `records.cpc` | Registro con un campo refinado (saldo nunca negativo) |
| `multiset.cpc` | **Multiset estilo `std::multiset`** sobre un árbol rojo-negro (left-leaning): insert, erase, erase-all, count, lower-bound, to-list |
| `fibheap.cpc` | **Fibonacci heap**: insert, min, extract-min con consolidación, decrease-key con cortes en cascada, delete |
| `linkcut.cpc` | **Link-cut tree** con splay trees: link, cut, find-root, connected?, path-sum, set |

### Estructuras de datos

Los tres programas grandes siguen el mismo esquema, porque el lenguaje no tiene mutación ni punteros:

- Los nodos viven en una **arena** de listas paralelas de longitud fija (64), dentro de un registro. Los "punteros" son índices de tipo `idx = { v : int | v >= 0 && v < 64 }` y el nodo 0 es el nulo.
- Cada operación recibe la estructura y devuelve la estructura actualizada; las que necesitan devolver además un nodo lo dejan en el campo `ret`.
- El verificador garantiza estáticamente que ningún `nth`/`set` sale de rango, que los contadores declarados como naturales nunca son negativos y que las tablas auxiliares (grados del heap) se indexan con prueba de rango. Varios errores reales del código se detectaron así al escribirlo.

```lisp
> :load ej/multiset.cpc
> (let ((m (ms-insert (ms-insert (ms-insert ms-new 5) 3) 5)))
    (list (ms-size m) (ms-count m 5) (get (ms-lower-bound m 4) key)))
[3, 2, 5] : { v : list int | len v == 3 }
```

```lisp
; ej/ariane.cpc
(let (aInt16 : { v : double | v >= -32768.0 && v <= 32767.0 } -> { v : int | v >= -32768 && v <= 32767 }
      (lambda : { v : double | v >= -32768.0 && v <= 32767.0 } -> { v : int | v >= -32768 && v <= 32767 } (h)
        (toInt h)))
  (aInt16 1234.5))
; => 1234 : { v : int | v >= -32768 && v <= 32767 }

; ej/failed_ariane.cpc  (velocidad : { v : double | v >= 0.0 })
; => Error: Type mismatch in function argument: expected { v : double | v >= -32768.0 && v <= 32767.0 }, got ...
```

**¡Disfruta programando en Cepuac!**
