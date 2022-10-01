##
## Sobre más features
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## <Insert a smart quote here about more is better>.
## --- Ale

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL
mayo$clase_ternaria <- NULL

## ---------------------------
## Step 2: XGBoost, un modelo simple ...
## ---------------------------

dtrain <- xgb.DMatrix(
        data = data.matrix(marzo),
        label = clase_binaria, missing = NA)

# Empecemos con algo muy básico
param_fe <- list(
            max_depth = 2,
            eta = 0.1,
            objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)

## ---------------------------
## Step 3: XGBoost, ... para generar nuevas variables
## ---------------------------

# https://research.facebook.com/publications/practical-lessons-from-predicting-clicks-on-ads-at-facebook/

new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))
colnames(new_features)[150:173]

## ---------------------------
## Step 4: Entendiendo como se construyen.
## ---------------------------

xgb.plot.tree(colnames(new_features), xgb_model, trees = 0)


## ---------------------------
## Step 5: Viendo cuán importantes son las nuevas variables, pero con un LGBM!!!
## ---------------------------

dtrain_lgb  <- lgb.Dataset(
            data = data.matrix(new_features),
            label = clase_binaria)

mlgb <- lgb.train(
            dtrain_lgb,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb)

## ---------------------------
## Step 6: Jugando un poco más con los parámetros del XGBoost
## ---------------------------

set.seed(semillas[1])
param_fe2 <- list(
                colsample_bynode = 0.8,
                learning_rate = 1,
                max_depth = 3, # <--- IMPORTANTE CAMBIAR
                num_parallel_tree = 10, # <--- IMPORTANTE CAMBIAR
                subsample = 0.8,
                objective = "binary:logistic"
            )

xgb_model2 <- xgb.train(params = param_fe2, data = dtrain, nrounds = 1)

# Veamos un paso a paso
new_features2 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

colnames(new_features2)[150:230]

dtrain_lgb2  <- lgb.Dataset(
            data = data.matrix(new_features2),
            label = clase_binaria)

mlgb2 <- lgb.train(
            dtrain_lgb2,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb2)$Feature

# Filtrando las features que entraron
## Preguntas
## - ¿Entraron todas las variables?

## ---------------------------
## Step 7: Sumando canaritos
## ---------------------------

set.seed(semillas[1])
for (i in 1:20)  {
    marzo[, paste0("canarito", i) := runif(nrow(marzo))]
}

new_features3 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

# Veamos que están las variables que generamos
colnames(new_features3)[150:230]

dtrain_lgb3  <- lgb.Dataset(
            data = data.matrix(new_features3),
            label = clase_binaria)

mlgb3 <- lgb.train(
            dtrain_lgb3,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05,
                num_iterations = 500 ## <-- aumento las iteraciones
            ),
            verbose = -1)

var_importance <- lgb.importance(mlgb3)$Feature

# Veamos cuantas canaritos aparecieron
list_canaritos <- grepl("canarito", var_importance)

# Cuantos canaritos aparecieron?
length(var_importance[list_canaritos])

# En que posiciones
idx <- seq(length(list_canaritos))
idx[list_canaritos]

# En que posiciones aprecieron el resto de las variables generadas
list_new_features <- grepl("V\\d+", var_importance)
idx[list_new_features]
