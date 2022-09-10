rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("rgenoud")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/tomas/workspace/uba/dmeyf")
# Poner sus semillas
semillas <- c(697157, 585799, 906007, 748301, 372871)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_original_2022.csv")

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
    clase_ternaria != "CONTINUA",
    "evento",
    "noevento"
)]

# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Probamos sacando la mejor variables
#dataset[, ctrx_quarter := NULL]    ID Exp 20 -> No funciona

# Arma secreta aportada por Gustavo Denicolay
#dataset[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
#dataset[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]
#
#dataset[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
#dataset[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]
#
#dataset[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
#dataset[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]
#
#dataset[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
#dataset[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

# Arma secreta con arbol excluyendo a ctrx_quarter
dataset[, cd_v2_1 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales < 14.851E+3)]
dataset[, cd_v2_2 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales >= 14.851E+3)]

dataset[, cd_v2_3 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo < 3572.7)]
dataset[, cd_v2_4 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo >= 3572.7)]

dataset[, cd_v2_5 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll < 4693.1)]
dataset[, cd_v2_6 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll >= 4693.1)]

dataset[, cd_v2_7 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen < 296.68)]
dataset[, cd_v2_8 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen >= 296.68)]

fwrite(dataset, "./datasets/competencia1_2022.csv")
