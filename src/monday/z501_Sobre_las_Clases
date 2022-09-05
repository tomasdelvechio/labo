##
## Sobre el Orden
##
## ---------------------------
## Step 1: Ejecutando un árbol
## ---------------------------
##
## The distinctions separating the ~~social~~ classes are false; in the last
## analysis they rest on force.
## --- Albert Einstein
##

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("treeClust")
require("ggplot2")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]


## ---------------------------
## Step 2: En busca de un poco del EDA perdido
## ---------------------------

# Veamos las variables más importantes
# ctrx_quarter
ggplot(dataset, aes(x = ctrx_quarter)) +
  facet_grid(clase_ternaria ~ .) +
  geom_density()

# Zoom por favor
ggplot(dataset[ctrx_quarter < 300, ], aes(x = ctrx_quarter)) +
  facet_grid(clase_ternaria ~ .) +
  geom_density()

# Cantidad de préstamos personales
dcast(dataset, cprestamos_personales  ~ clase_ternaria,
                length,
                value.var = "clase_ternaria")

ggplot(dataset[mcuentas_saldo > 0 &
                mcuentas_saldo < 10000, ], aes(x = mcuentas_saldo)) +
  facet_grid(clase_ternaria ~ .) +
  geom_histogram()

## Preguntas
## ¿Cómo funciona una árbol de decisión?
## ¿Afecta a la separación de los árboles que las distribuciones sean similares?

## ---------------------------
## Step 3: Mezclando clases.
## ---------------------------

# Armamos diferentes clases binarias:
# Sólo es evento las clase BAJA+2
dataset[, clase_binaria1 := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

# Clases BAJAS+1 y BAJA+2 combinadas
dataset[, clase_binaria2 := ifelse(
                            clase_ternaria == "CONTINUA",
                                "noevento",
                                "evento"
                            )]

# Vamos a probar que tal anda un modelo con las clases combinadas 
set.seed(semillas[1])
in_training <- caret::createDataPartition(dataset$clase_binaria2,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

# Usamos parámetros ~~robados~~ sacados de los scripts de Gustavo.
parametros <- list(cp = -1, minsplit = 1073, minbucket = 278, maxdepth = 9)
# MUY IMPORTANTE: No estamos sacando los otros targets del dataset, hay
# que sacarlos en la fórmula como esta debajo.
modelo_bin2 <- rpart(clase_binaria2 ~ . - clase_ternaria - clase_binaria1,
                data = dtrain,
                xval = 0,
                control = parametros)

# Y calculamos la ganancia
pred_testing <- predict(modelo_bin2, dtest, type = "prob")
print(sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(
                    # Usamos la clase ternaria para calcular la gan
                    dtest$clase_ternaria == "BAJA+2",
                    78000,
                    -2000) / 0.3
    )
)

## Preguntas
## ¿Obtuvo una importante mejora en su modelo?
## ¿Qué detalle fue creado con la clase BAJA+2 en mente que ya no aplica?

## ---------------------------
## Step 4: De árboles a tablas, la venganza
## ---------------------------

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# sobre un modelo. Es una "mejora" sobre la función dada en el script z201:
# - El target ahora debe ser binario: evento/noevento
# - Calcula la ganancia en base
# - Ordena de mayor a menor ganancia
# - Agrega un prefijo para poder juntar diferentes resultados.

# hoja train/test(continua baja+1 baja+2 evento noevento prob ganancia)
leaves_table <- function(model, train, target, prefix = "") {
    leaves_train_table <- data.table(
        # Devuelve en que hoja cae un caso
        leaves = rpart.predict.leaves(model, train, type = "where"),
        classes = train[, clase_ternaria],
        target = train[, get(target)]
    )
    leaves <- dcast(
            leaves_train_table,
            leaves ~ classes, length,
            value.var = "target")
    leaves <- leaves[
        dcast(
        leaves_train_table,
        leaves ~ target, length,
        value.var = "target"),
        on = .(leaves)]
    leaves[, n := evento + noevento]
    leaves[, p := round(evento / n,4)]
    leaves <- leaves[order(-p),]
    leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
    leaves[, ':='(evento = NULL, noevento = NULL)]
    setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
                    new = c(paste0(prefix, "b1"),
                            paste0(prefix, "b2"),
                            paste0(prefix, "cont"),
                            paste0(prefix, "n"),
                            paste0(prefix, "p"),
                            paste0(prefix, "gan")))
    leaves[]
}

# Examinamos las nuevas hojas de nuestro modelo para entender las nuevas
# probabilidad. Primero sobre TRAIN

train_bin2 <- leaves_table(modelo_bin2, dtrain, "clase_binaria2")
print(train_bin2)

## Preguntas
## ¿Sigue siendo el punto de corte optimo 0.025?
## ¿Dejamos plata sobre la mesa?

## ---------------------------
## Step 4: Contando la plata que nos dejamos
## ---------------------------

train_bin2[, gan_acum := cumsum(gan) / 0.7]
train_bin2[, n_acum := cumsum(n) / 0.7]

print(train_bin2)

# La ganancia en train para el punto de corte de 0.025 es
train_bin2[p >= 0.025, sum(gan) / 0.7]

# Podemos buscar el punto de corte optimo con un par de sentencias de R
pos_max_gan <- which.max(train_bin2$gan_acum)
# La ganancia máxima
train_bin2[pos_max_gan, gan_acum]

# La probabilidad que da esa ganancia
train_bin2[pos_max_gan, p]

# La cantidad de envíos normalizados para esa ganancia 
train_bin2[pos_max_gan, n_acum / 0.7]

## Preguntas
## ¿Es útil con su semilla este mezcla de clases?
## ¿Qué desafíos ve en este punto?

## ---------------------------
## Off topic: Si jugamos con clases, jugemos en serio
## ---------------------------

# Saquemos los registros de la clase BAJA+1 para entrenar, porque la C1
# también esta lleno de loquitos...

modelo_sin_b1 <- rpart(clase_binaria2 ~ . - clase_ternaria - clase_binaria1,
                data = dtrain[dtrain$clase_ternaria != "BAJA+1", ],
                xval = 0,
                control = parametros)

train_sin_b1 <- leaves_table(modelo_sin_b1, dtrain, "clase_binaria2")
train_sin_b1[, gan_acum := cumsum(gan) / 0.7]
train_sin_b1[, n_acum := cumsum(n)]
train_sin_b1

## Sin preguntas ni comentarios

## ---------------------------
## Step 5: Entendiendo mejor los cortes. Sumemos test al análisis
## ---------------------------

# Saquemos las hojas para test y juntemos todo en una tabla para analizar
test_bin2 <- leaves_table(modelo_bin2, dtest, "clase_binaria2", "te_")
res_bin2 <- train_bin2[test_bin2, on = .(leaves)]
res_bin2 <- res_bin2[order(-p)]

res_bin2[, te_gan_acum := cumsum(te_gan) / 0.3]
res_bin2[, te_n_acum := cumsum(te_n) / 0.3]

# Solo veamos algunas columnas
print(res_bin2[, c("p", "n", "gan", "gan_acum", "te_n", "te_gan",
                  "te_gan_acum", "te_n_acum")])

## Preguntas
## ¿Se mantiene el punto de corte en train como en test?
## ¿Si tuviera que tomar una decisión, seguía usando el que dió train?

## ---------------------------
## Step 6: Iterando sobre múltiples semillas
## ---------------------------

# Vamos a hacer un análisis un poco más abierto para elegir un punto de corte


for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset$clase_binaria2,
                        p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]

    modelo_bin2 <- rpart(clase_binaria2 ~ . - clase_ternaria - clase_binaria1,
                    data = dtrain,
                    xval = 0,
                    control = parametros)

    train_bin2 <- leaves_table(modelo_bin2, dtrain, "clase_binaria2")

    train_bin2[, gan_acum := cumsum(gan) / 0.7]

    test_bin2 <- leaves_table(modelo_bin2, dtest, "clase_binaria2", "te_")

    res_bin2 <- train_bin2[test_bin2, on = .(leaves)]
    res_bin2 <- res_bin2[order(-p)]

    res_bin2[, te_gan_acum := cumsum(te_gan) / 0.3]
    res_bin2[, te_n_acum := cumsum(te_n) / 0.3]

    pos_max_gan_train <- which.max(res_bin2$gan_acum)
    pos_max_gan_test <- which.max(res_bin2$te_gan_acum)

    print(sprintf("Mejor gan_acum_train: %i - Mejor gan_acum_test: %i",
            pos_max_gan_train,
            pos_max_gan_test))
    print(res_bin2[
            sort(seq(pos_max_gan_train, pos_max_gan_test))
            , c("p", "gan", "gan_acum", "te_n", "te_gan",
                "te_gan_acum", "te_n_acum")])

}

## Agregue estadísticos para tomar una mejor decisión.

## Preguntas - Active Learning
## - ¿Qué estrategia piensa que puede ser útil para elegir el punto de corte?
## - ¿Para la búsqueda del mejor modelo que valor va a usar para la OB?
## - ¿Pros y contras de elegir los N mejores casos?
