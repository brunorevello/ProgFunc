#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FW=${FW:-"$SCRIPT_DIR/FWhile"}
cd "$SCRIPT_DIR/tests" || { echo "No se encontró la carpeta tests/"; exit 1; }

echo "# Pretty printing"
for fw in ./pp-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"

    timeout 10s "$FW" "$fw" -p > "$sal"
    status=$?

    if [ $status -eq 124 ]; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done

echo "# Chequeo de nombres"
for fw in ./nc-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"

    timeout 10s "$FW" "$fw" > "$sal"
    status=$?

    if [ $status -eq 124 ]; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done

echo "# Chequeo de tipos"
for fw in ./ty-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"

    timeout 10s "$FW" "$fw" > "$sal"
    status=$?

    if [ $status -eq 124 ]; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done

echo "# Evaluacion"
for fw in ./eval-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"
    in="${base}.in"

    : > "$sal"
    timed_out=false

    while IFS= read -r linea; do
        timeout 40s "$FW" "$fw" -e "$linea" >> "$sal"
        status=$?
        if [ $status -eq 124 ]; then
            timed_out=true
            break
        fi
    done < "$in"

    if $timed_out; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done

echo "# Evaluacion con errores"
for fw in ./evalerr-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"
    in="${base}.in"

    : > "$sal"
    timed_out=false

    while IFS= read -r linea; do
        timeout 40s "$FW" "$fw" -e "$linea" >> "$sal"
        status=$?
        if [ $status -eq 124 ]; then
            timed_out=true
            break
        fi
    done < "$in"

    if $timed_out; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done

echo "# Generales"
for fw in ./gen-*.fw; do
    [ -e "$fw" ] || continue
    base="${fw#./}"
    base="${base%.fw}"
    sal="${base}.sal"
    out="${base}.out"
    in="${base}.in"

    : > "$sal"
    timed_out=false

    if [ -e "$in" ]; then
        while IFS= read -r linea; do
            timeout 10s "$FW" "$fw" -e "$linea" >> "$sal"
            status=$?
            if [ $status -eq 124 ]; then
                timed_out=true
                break
            fi
        done < "$in"
    else
        if [ "$base" = "gen-03" ]; then
            timeout 10s "$FW" "$fw" -p > "$sal"
        else
            timeout 10s "$FW" "$fw" > "$sal"
        fi
        status=$?
        if [ $status -eq 124 ]; then
            timed_out=true
        fi
    fi

    if $timed_out; then
        : > "$sal"
        echo "$base: TIMEOUT"
    elif diff -u "$out" "$sal" > /dev/null; then
        echo "$base: OK"
    else
        echo "$base: FALLO"
        diff -u "$out" "$sal"
    fi
done
