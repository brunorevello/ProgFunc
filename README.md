# ProgFunc - Laboratorio de Programación Funcional 2026

Repositorio del proyecto realizado por **Ignacio Firpo** y **Bruno Revello**.

## Instalación y Compilación

Para compilar el proyecto, asegúrate de tener instalado GHC (Glasgow Haskell Compiler). Ejecuta el siguiente comando en la raíz del proyecto para generar el binario:

```bash
ghc -hide-all-packages -package base -package parsec FWhile.hs

```

---

## Ejecución de Tests

El proyecto incluye un script de automatización `runtests.sh` para verificar el correcto funcionamiento del intérprete.

### Uso básico

Por defecto, al ejecutar el script sin argumentos, se ejecutarán **todos** los grupos de pruebas:

```bash
./runtests.sh

```

### Opciones personalizadas

Puedes filtrar qué grupos de tests ejecutar utilizando las siguientes banderas:

| Flag | Descripción |
| --- | --- |
| `-p` | Pretty Printing |
| `-n` | Chequeo de nombres |
| `-t` | Chequeo de tipos |
| `-e` | Evaluación |

**Ejemplo:** Para ejecutar únicamente los tests de *Pretty printing* y *Chequeo de tipos*, utiliza:

```bash
./runtests.sh -p -t

```

---

## Estructura del Proyecto

* `FWhile.hs`: Código fuente principal del intérprete.
* `TypeChecker.hs`: Módulo para el chequeo de tipos y nombres.
* `tests/`: Carpeta que contiene los archivos de prueba (`.fw`) y sus resultados esperados.
* `runtests.sh`: Script de automatización de pruebas.

