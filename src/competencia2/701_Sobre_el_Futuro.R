rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

setwd("/home/tomas/workspace/uba/dmeyf")
semillas <- c(697157, 585799, 906007, 748301, 372871)

##### Exp 1

param <- list()

param$input$dataset <- "./exp/FE7110/dataset_7110.csv.gz"

param$finalmodel$max_bin <- 31
param$finalmodel$num_iterations <- 970 # 615
param$finalmodel$semilla <- 697157
param$finalmodel$learning_rate <- 0.005301229450038 # 0.0142501265
param$finalmodel$feature_fraction <- 0.307355765531438 # 0.8382482539
param$finalmodel$min_data_in_leaf <- 107 # 5628
param$finalmodel$num_leaves <- 494 # 784

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread(param$input$dataset, stringsAsFactors = TRUE)
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "CONTINUA", 0, 1)
enero$clase_ternaria <- NULL

## ---------------------------
## Step 2: Un modelo simple de LGBM
## ---------------------------

# Armamos el dataset de train para LGBM
dtrain <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

model_lgm <- lgb.train(
    data = dtrain,
    param = list(
        objective = "binary",
        max_bin = param$finalmodel$max_bin,
        learning_rate = param$finalmodel$learning_rate,
        num_iterations = param$finalmodel$num_iterations,
        num_leaves = param$finalmodel$num_leaves,
        min_data_in_leaf = param$finalmodel$min_data_in_leaf,
        feature_fraction = param$finalmodel$feature_fraction,
        seed = param$finalmodel$semilla
    ),
    verbose = -1
)

## ---------------------------
## Step 3: Veamos como funcionó en Marzo
## ---------------------------

marzo$pred <- predict(model_lgm, data.matrix(marzo[, 1:197]))
sum((marzo$pred > 0.025) * ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))

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
m <- 500 # salto de cnatidad de los envios a probar
f <- 2000 # desde
t <- 12000 # hasta

leaderboad <- data.table()
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)
marzo$board[-split] <- "publico_exp1"
marzo$board[split] <- "privado_exp1"
for (s in seq(f, t, m)) {
    privado <- marzo[1:s, sum(ifelse(board == "privado_exp1",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0
    )) / 0.5]
    publico <- marzo[1:s, sum(ifelse(board == "publico_exp1",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0
    )) / 0.5]
    leaderboad <- rbindlist(list(
        leaderboad,
        data.table(envio = s, board = "privado_exp1", valor = privado),
        data.table(envio = s, board = "publico_exp1", valor = publico)
    ))
}
# Graficamos
ggplot(leaderboad[board == "publico_exp1"], aes(x = envio, y = valor, color = board)) +
    geom_line()

ggplot(leaderboad, aes(x = envio, y = valor, color = board)) +
    geom_line()




##### Exp 2

param <- list()

param$input$dataset <- "./exp/FE7110/dataset_7110.csv.gz"

param$finalmodel$max_bin <- 31
param$finalmodel$num_iterations <- 1021 # 615
param$finalmodel$semilla <- 697157
param$finalmodel$learning_rate <- 0.030650734362149 # 0.0142501265
param$finalmodel$feature_fraction <- 0.766895859756822 # 0.8382482539
param$finalmodel$min_data_in_leaf <- 822 # 5628
param$finalmodel$num_leaves <- 256 # 784

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread(param$input$dataset, stringsAsFactors = TRUE)
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "CONTINUA", 0, 1)
enero$clase_ternaria <- NULL

## ---------------------------
## Step 2: Un modelo simple de LGBM
## ---------------------------

# Armamos el dataset de train para LGBM
dtrain <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

model_lgm <- lgb.train(
    data = dtrain,
    param = list(
        objective = "binary",
        max_bin = param$finalmodel$max_bin,
        learning_rate = param$finalmodel$learning_rate,
        num_iterations = param$finalmodel$num_iterations,
        num_leaves = param$finalmodel$num_leaves,
        min_data_in_leaf = param$finalmodel$min_data_in_leaf,
        feature_fraction = param$finalmodel$feature_fraction,
        seed = param$finalmodel$semilla
    ),
    verbose = -1
)

## ---------------------------
## Step 3: Veamos como funcionó en Marzo
## ---------------------------

marzo$pred <- predict(model_lgm, data.matrix(marzo[, 1:197]))
sum((marzo$pred > 0.025) * ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))

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
m <- 500 # salto de cnatidad de los envios a probar
f <- 2000 # desde
t <- 12000 # hasta

#leaderboad <- data.table()
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)
marzo$board[-split] <- "publico_exp2"
marzo$board[split] <- "privado_exp2"
for (s in seq(f, t, m)) {
    privado <- marzo[1:s, sum(ifelse(board == "privado_exp2",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0
    )) / 0.5]
    publico <- marzo[1:s, sum(ifelse(board == "publico_exp2",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0
    )) / 0.5]
    leaderboad <- rbindlist(list(
        leaderboad,
        data.table(envio = s, board = "privado_exp2", valor = privado),
        data.table(envio = s, board = "publico_exp2", valor = publico)
    ))
}
# Graficamos
ggplot(leaderboad[board == "publico_exp2"], aes(x = envio, y = valor, color = board)) +
    geom_line()

ggplot(leaderboad, aes(x = envio, y = valor, color = board)) +
    geom_line()
