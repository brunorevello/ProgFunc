#!/usr/bin/env bash

# Directorio donde están los tests
DIR="tests"

# Binario del intérprete FWhile.
FW=${FW:-./FWhile}

run_diff_tests() {
    local titulo="$1"
    local patron="$2"
    shift 2
    local args=("$@")

    echo "# $titulo"

    # Buscamos usando el prefijo $DIR/
    for fw in "$DIR"/$patron; do
        [ -e "$fw" ] || continue

        local base="${fw%.fw}"
        local sal="${base}.sal"
        local out="${base}.out"

        "$FW" "$fw" "${args[@]}" > "$sal"

        if diff -u "$out" "$sal" > /dev/null; then
            echo "$(basename "$base"): OK"
            # Opcional: borrar el .sal si quieres mantener todo limpio
            # rm "$sal" 
        else
            echo "$(basename "$base"): FAIL"
            diff -u "$out" "$sal"
        fi
    done
}

run_eval_tests() {
    echo "# Evaluacion"

    for fw in "$DIR"/eval-*.fw; do
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
            echo "$(basename "$base"): OK"
        else
            echo "$(basename "$base"): FAIL"
            diff -u "$out" "$sal"
        fi
    done
}

# ----------------------------------------------------------------------
# Opciones (sin cambios)
# ----------------------------------------------------------------------
# ... (deja esta parte igual que la tenías)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Ejecución de los tests
# ----------------------------------------------------------------------

$do_pp   && run_diff_tests "Pretty printing"     "pp-*.fw"   -p
$do_nc   && run_diff_tests "Chequeo de nombres"  "nc-*.fw"
$do_ty   && run_diff_tests "Chequeo de tipos"    "ty-*.fw"
$do_eval && run_eval_tests