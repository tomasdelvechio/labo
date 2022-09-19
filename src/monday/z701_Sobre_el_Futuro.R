##
## Sobre El Futuro
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## The future is not something to predict. The future is something to build.
## --- Franco Ongaro

# Profundizaremos en los puntos de corte.
# IMPORTANTE: En esta competencia se puede entrenar usando Marzo. Sin embargo,
# vamos aprovechar (y recomendar que usted también lo haga) Enero para
# experimentar contra Marzo.

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "CONTINUA", 0, 1)
enero$clase_ternaria <- NULL

## ---------------------------
## Step 2: Un modelo simple de LGBM
## ---------------------------

# Armamos el dataset de train para LGBM
dtrain  <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

model_lgm <- lightgbm(data = dtrain,
            nrounds = 100,
            params = list(objective = "binary",
                          max_bin = 15,
                          min_data_in_leaf = 4000,
                          learning_rate = 0.05),
             verbose = -1)

## ---------------------------
## Step 3: Veamos como funcionó en Marzo
## ---------------------------

marzo$pred <- predict(model_lgm, data.matrix(marzo[, 1:154]))
sum((marzo$pred > 0.025) * ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))

## ---------------------------
## Step 4: Veamos cuán distintos los scores entregados
## ---------------------------

length(marzo$pred)
length(unique(marzo$pred))

## Preguntas
## - ¿Qué diferencia observa con respecto a ?

## ---------------------------
## Step 4: En el leaderboard público.
## ---------------------------

# Simulamos un Leaderboard público:
set.seed(semillas)
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)

# Vemos la cantidad de casos que estaríamos mandando:clase_ternaria
sum(marzo$pred > 0.025) # En mi caso dice que estaría mandando 7744

# Y obtendríamos una ganancia de
# Privado
sum((marzo$pred[split] > 0.025) * ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.5

# Público
sum((marzo$pred[-split] > 0.025) * ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.5

# Pero... que pasa si mandamos otra cantidad de casos?
# Vamos a mandar los N mejores casos, de a separaciones de M

## ---------------------------
## Step 4: Buscando el mejor punto de corte en el leaderboard público.
## ---------------------------

# Ordenamos el dataset segun su probabilidad de forma ascendente
setorder(marzo, cols = -pred)

# PROBAR MULTIPLES VALORES
set.seed(semillas[3])
m <- 500
f <- 2000
t <- 12000

leaderboad <- data.table()
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)
marzo$board[split] <- "privado"
marzo$board[-split] <- "publico"
for (s in seq(f, t, m)) {
    privado <- marzo[1:s, sum(ifelse(board == "privado",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    publico <- marzo[1:s, sum(ifelse(board == "publico",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    leaderboad <- rbindlist(list(leaderboad,
                        data.table(envio = s, board = "privado", valor = privado),
                        data.table(envio = s, board = "publico", valor = publico)
                        ))
}
# Graficamos
ggplot(leaderboad, aes(x = envio, y = valor, color = board)) + geom_line()

## ACTIVE LEARNING: Juegue con los parámetros y busque si hay alguna información
## en el leaderboard público que le de una estrategia para elegir la cantidad
## adecuada para ganar maximizar la ganancia del privado.

