#!/usr/bin/env bash


# Script para ejecutar los tests de FWhile.
#
# Por defecto se ejecutan todos los tests.
# Se puede indicar mediante flags qué grupos de tests ejecutar.
#
# Opciones:
#   -p   Pretty printing.
#   -n   Chequeo de nombres.
#   -t   Chequeo de tipos.
#   -e   Evaluación.
#   -e   Evaluación.
#   -r   Evaluación con errores.
#
# Por ejemplo:
#   ./runtests.sh -p -t
# Ejecuta únicamente los tests de pretty printing y chequeo de tipos.


# Binario del intérprete FWhile.
FW=${FW:-./FWhile}

run_diff_tests() {
    local titulo="$1"
    local patron="$2"
    shift 2
    local args=("$@")

    echo "# $titulo"

    for fw in $patron; do
        [ -e "$fw" ] || continue

        local base="${fw%.fw}"
        local sal="${base}.sal"
        local out="${base}.out"

        "$FW" "$fw" "${args[@]}" > "$sal"

        if diff -u "$out" "$sal" > /dev/null; then
            echo "$base: OK"
        else
            echo "$base: FAIL"
            diff -u "$out" "$sal"
        fi
    done
}

run_eval_tests() {
    local err="$1"
    echo "# Evaluacion"

    for fw in eval$err-*.fw; do
        [ -e "$fw" ] || continue

        local base="${fw%.fw}"
        local sal="${base}.sal"
        local out="${base}.out"
        local in="${base}.in"

        : > "$sal"

        while IFS= read -r linea; do
            "$FW" "$fw" -e "$linea" >> "$sal"
        done < "$in"

        if diff -u "$out" "$sal" > /dev/null; then
            echo "$base: OK"
        else
            echo "$base: FAIL"
            diff -u "$out" "$sal"
        fi
    done
}

# ----------------------------------------------------------------------
# Opciones
# ----------------------------------------------------------------------

do_pp=false
do_nc=false
do_ty=false
do_eval=false
do_evalerr=false

while getopts "pnter" opt; do
    case "$opt" in
        p) do_pp=true ;;
        n) do_nc=true ;;
        t) do_ty=true ;;
        e) do_eval=true ;;
        r) do_evalerr=true ;;
        *)
            echo "Uso: $0 [-p] [-n] [-t] [-e] [-r]"
            exit 1
            ;;
    esac
done

# Por defecto se ejecuta todo
if ! $do_pp && ! $do_nc && ! $do_ty && ! $do_eval && ! $do_evalerr; then
    do_pp=true
    do_nc=true
    do_ty=true
    do_eval=true
    do_evalerr=true
fi

# ----------------------------------------------------------------------
# Ejecución de los tests
# ----------------------------------------------------------------------

$do_pp   && run_diff_tests "Pretty printing"     "pp-*.fw"   -p
$do_nc   && run_diff_tests "Chequeo de nombres"  "nc-*.fw"
$do_ty   && run_diff_tests "Chequeo de tipos"    "ty-*.fw"
$do_eval && run_eval_tests ""
$do_evalerr && run_eval_tests "err"
