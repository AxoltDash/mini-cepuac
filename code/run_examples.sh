#!/bin/sh
# Ejecuta todos los programas de ej/ y muestra la ultima linea de salida de
# cada uno (el resultado final o el error).
# Los archivos que empiezan con "failed_" o "invalid_" deben producir Error.
# Uso: ./run_examples.sh   (desde code/)
cd "$(dirname "$0")"
for f in ej/*.cpc; do
  out=$(printf ':load %s\n' "$f" | runghc -isrc Cepuac.hs 2>&1 | sed -n '11,$p' | sed 's/^> //' | grep -v '^Bye\.$' | tail -1)
  printf '%-26s %s\n' "$f" "$out"
done
