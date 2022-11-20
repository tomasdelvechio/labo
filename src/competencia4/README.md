# Planificación competencia 4

Los scripts de este directorio se ejecutarán en el siguiente orden

- z906
- z914
- z925
- z932
- z942
- z992 * n (Puede haber muchos de estos; leer abajo)
- z1292

## ¿Porque tantos scripts z992?

El objetivo es paralelizar la ejecución de los semillerios. Incluso, para cada semillerio, la idea es paralelizar la ejecución de cada semilla.

Dentro de cada script, siguiendo el esquema de la asignatura, entenderemos los PARAM relevantes:

- PARAM$modelo indica que modelo de la OB ordenada por ganancia, tomaremos para la ejecución. Para muchas semillas que usen el mismo PARAM$modelo, diremos que son semillas del mismo semillerio. Ademas, procuraremos que sus salidas terminen en el mismo directorio.

- Dentro del script existe la variable PARAM$semillerio. Indica el tamaño del semillerio, en otras palabras, la cantidad de semillas que nos vamos a permitir incluir en el mismo. Todos los scripts son entregados para armar semillerios de 100 semillas, pero eso puede ser cambiado por este parametro.

- El tamaño REAL del semillerio estará dado por la cantidad de semillas que lleguemos a procesar, no por las que deseamos. Por eso, sabiendo que no siempre llegaremos a las 100 corridas de cada semilla, porque la VM es spot, o porque por tiempo queremos paralelizar esto, se crearon 2 variables complementarias: PARAM$indice_inicio_semilla y PARAM$indice_fin_semilla. Indican dentro del array de 100 semillas, cuales usará ese script.

## Ejemplos de configuraciones

**Ejemplo 1**

```r
PARAM <- list()
PARAM$experimento <- "ZZ9410_semillerio_ensamble_m1"
PARAM$exp_input <- "HT9420_C4_exp1"

PARAM$modelo <- 11
PARAM$semilla_primos <- 42
PARAM$semillerio <- 100
PARAM$indice_inicio_semilla <- 21
PARAM$indice_fin_semilla <- 50
```

Este ejemplo tomará la OB que este en el dir `HT9420_C4_exp1`, usará el onceavo mejor modelo de la misma, generará un listado de 100 números primos aleatorios (usando la semilla 42), y el script procesara 29 semillas (desde la 21 a la 50).

Deberá existir un script anterior a este, con la config

```r
PARAM <- list()
PARAM$experimento <- "ZZ9410_semillerio_ensamble_m1"
PARAM$exp_input <- "HT9420_C4_exp1"

PARAM$modelo <- 11
PARAM$semilla_primos <- 42
PARAM$semillerio <- 100
PARAM$indice_inicio_semilla <- 1   # CAMBIO SOLO ESTO
PARAM$indice_fin_semilla <- 20     # CAMBIO SOLO ESTO
```

El único cambio es en los indices, que ahora van del 1 al 20. Ambos envian la salida al mismo dir `ZZ9410_semillerio_ensamble_m1`, por lo que el script z1292 tomará este y construira un semillerio a partir de estos 2 scripts.

Nada impide armar un tercer scripts que se parametrice así

```r
PARAM <- list()
PARAM$experimento <- "ZZ9410_semillerio_ensamble_m1"
PARAM$exp_input <- "HT9420_C4_exp1"

PARAM$modelo <- 11
PARAM$semilla_primos <- 42
PARAM$semillerio <- 100
PARAM$indice_inicio_semilla <- 51   # CAMBIO SOLO ESTO
PARAM$indice_fin_semilla <- 80      # CAMBIO SOLO ESTO
```

Complementará el mismo semillerio, cambiando los indices.

## ¿Los scripts de este repo?

Bueno, es mi plan de semillerios.

Cada Modelo esta dividido en 4 archivos

Llamé modelo 1 al que va de `z992_ZZ_lightgbm_under_semillerio_1.r` a `z992_ZZ_lightgbm_under_semillerio_4.r`, modelo 2 al que va de `_5` a `_8`, modelo 3 es el que abarca de `_9` a `_12` y por último, modelo 4 incluye `_13` a `_16`.

Si miran, dentro de cada "modelo", PARAM$modelo permanece igual, de la misma forma que PARAM$experimento. Cambian los indices menor y mayor, para poder paralelizar.