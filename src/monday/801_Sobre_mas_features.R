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
require("dplyr")
require("Hmisc")

# Poner la carpeta de la materia de SU computadora local
#setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
#semillas <- c(17, 19, 23, 29, 31)

setwd("/home/tomas/workspace/uba/dmeyf")
semillas  <- c(697157, 585799, 906007, 748301, 372871)

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

## Arma secreta aportada por Gustavo Denicolay
marzo[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
marzo[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]

marzo[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
marzo[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]

marzo[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
marzo[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]

marzo[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
marzo[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

## Arma secreta con arbol excluyendo a ctrx_quarter
marzo[, cd_v2_1 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales < 14.851E+3)]
marzo[, cd_v2_2 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales >= 14.851E+3)]

marzo[, cd_v2_3 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo < 3572.7)]
marzo[, cd_v2_4 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo >= 3572.7)]

marzo[, cd_v2_5 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll < 4693.1)]
marzo[, cd_v2_6 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll >= 4693.1)]

marzo[, cd_v2_7 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen < 296.68)]
marzo[, cd_v2_8 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen >= 296.68)]

## Features de prueba

marzo[, campo_prueba1 := ctrx_quarter * mcuentas_saldo]
marzo[, campo_prueba2 := ctrx_quarter * mcuenta_corriente]
marzo[, campo_prueba3 := ctrx_quarter * mprestamos_personales]
marzo[, campo_prueba4 := ctrx_quarter * ccomisiones_otras]
marzo[, campo_prueba5 := ctrx_quarter * active_quarter]
marzo[, campo_prueba6 := mcuentas_saldo * ctrx_quarter]
marzo[, campo_prueba7 := mcuentas_saldo * mcuenta_corriente]
marzo[, campo_prueba8 := mcuentas_saldo * mprestamos_personales]
marzo[, campo_prueba9 := mcuentas_saldo * ccomisiones_otras]
marzo[, campo_prueba10 := mcuentas_saldo * active_quarter]
marzo[, campo_prueba11 := mcuenta_corriente * ctrx_quarter]
marzo[, campo_prueba12 := mcuenta_corriente * mcuentas_saldo]
marzo[, campo_prueba13 := mcuenta_corriente * mprestamos_personales]
marzo[, campo_prueba14 := mcuenta_corriente * ccomisiones_otras]
marzo[, campo_prueba15 := mcuenta_corriente * active_quarter]
marzo[, campo_prueba16 := mprestamos_personales * ctrx_quarter]
marzo[, campo_prueba17 := mprestamos_personales * mcuentas_saldo]
marzo[, campo_prueba18 := mprestamos_personales * mcuenta_corriente]
marzo[, campo_prueba19 := mprestamos_personales * ccomisiones_otras]
marzo[, campo_prueba20 := mprestamos_personales * active_quarter]
marzo[, campo_prueba21 := ccomisiones_otras * ctrx_quarter]
marzo[, campo_prueba22 := ccomisiones_otras * mcuentas_saldo]
marzo[, campo_prueba23 := ccomisiones_otras * mcuenta_corriente]
marzo[, campo_prueba24 := ccomisiones_otras * mprestamos_personales]
marzo[, campo_prueba25 := ccomisiones_otras * active_quarter]
marzo[, campo_prueba26 := active_quarter * ctrx_quarter]
marzo[, campo_prueba27 := active_quarter * mcuentas_saldo]
marzo[, campo_prueba28 := active_quarter * mcuenta_corriente]
marzo[, campo_prueba29 := active_quarter * mprestamos_personales]
marzo[, campo_prueba30 := active_quarter * ccomisiones_otras]

## Bineado de features

variables_a_rankear <- c(
    "ctrx_quarter",
    "mprestamos_personales",
    "mcuentas_saldo",
    "mactivos_margen",
    "mcaja_ahorro",
    "mcuenta_corriente",
    "ccomisiones_otras",
    "active_quarter",
    "cprestamos_personales",
    "mcomisiones_mantenimiento",
    "mrentabilidad",
    "Visa_status",
    "mpasivos_margen",
    "cdescubierto_preacordado",
    "internet",
    "ccomisiones_mantenimiento"
)

prefix <- "r_"
for (var in variables_a_rankear) {
    marzo[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
}

# interacción entre variables

nuevas <- c()
for (var1 in variables_a_rankear) {
    for (var2 in variables_a_rankear) {
        if (var1 != var2) {
            nueva <- paste(var1, var2, sep = "___")
            marzo[, (nueva) := get(var1) * get(var2)]
            nuevas <- c(nuevas, nueva)
        }
    }
}

for (campo in colnames(marzo)) {
    if (marzo[, length(unique(get(campo))) > 100]) {
        marzo[, paste0(campo, "_bin") := as.integer(cut2(marzo[, get(campo)], m = 1, g = 63))]
        if (campo != "numero_de_cliente") marzo[, paste0(campo) := NULL]
        #cat(campo, " ")
    }
}


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
for (i in 1:40)  {
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
