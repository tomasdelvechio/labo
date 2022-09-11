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

## Arma secreta aportada por Gustavo Denicolay
dataset[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
dataset[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]

dataset[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
dataset[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]

dataset[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
dataset[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]

dataset[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
dataset[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

## Arma secreta con arbol excluyendo a ctrx_quarter
dataset[, cd_v2_1 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales < 14.851E+3)]
dataset[, cd_v2_2 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales >= 14.851E+3)]

dataset[, cd_v2_3 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo < 3572.7)]
dataset[, cd_v2_4 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo >= 3572.7)]

dataset[, cd_v2_5 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll < 4693.1)]
dataset[, cd_v2_6 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll >= 4693.1)]

dataset[, cd_v2_7 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen < 296.68)]
dataset[, cd_v2_8 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen >= 296.68)]

## Features de prueba

dataset[, campo_prueba1 := ctrx_quarter * mcuentas_saldo]
dataset[, campo_prueba2 := ctrx_quarter * mcuenta_corriente]
dataset[, campo_prueba3 := ctrx_quarter * mprestamos_personales]
dataset[, campo_prueba4 := ctrx_quarter * ccomisiones_otras]
dataset[, campo_prueba5 := ctrx_quarter * active_quarter]
dataset[, campo_prueba6 := mcuentas_saldo * ctrx_quarter]
dataset[, campo_prueba7 := mcuentas_saldo * mcuenta_corriente]
dataset[, campo_prueba8 := mcuentas_saldo * mprestamos_personales]
dataset[, campo_prueba9 := mcuentas_saldo * ccomisiones_otras]
dataset[, campo_prueba10 := mcuentas_saldo * active_quarter]
dataset[, campo_prueba11 := mcuenta_corriente * ctrx_quarter]
dataset[, campo_prueba12 := mcuenta_corriente * mcuentas_saldo]
dataset[, campo_prueba13 := mcuenta_corriente * mprestamos_personales]
dataset[, campo_prueba14 := mcuenta_corriente * ccomisiones_otras]
dataset[, campo_prueba15 := mcuenta_corriente * active_quarter]
dataset[, campo_prueba16 := mprestamos_personales * ctrx_quarter]
dataset[, campo_prueba17 := mprestamos_personales * mcuentas_saldo]
dataset[, campo_prueba18 := mprestamos_personales * mcuenta_corriente]
dataset[, campo_prueba19 := mprestamos_personales * ccomisiones_otras]
dataset[, campo_prueba20 := mprestamos_personales * active_quarter]
dataset[, campo_prueba21 := ccomisiones_otras * ctrx_quarter]
dataset[, campo_prueba22 := ccomisiones_otras * mcuentas_saldo]
dataset[, campo_prueba23 := ccomisiones_otras * mcuenta_corriente]
dataset[, campo_prueba24 := ccomisiones_otras * mprestamos_personales]
dataset[, campo_prueba25 := ccomisiones_otras * active_quarter]
dataset[, campo_prueba26 := active_quarter * ctrx_quarter]
dataset[, campo_prueba27 := active_quarter * mcuentas_saldo]
dataset[, campo_prueba28 := active_quarter * mcuenta_corriente]
dataset[, campo_prueba29 := active_quarter * mprestamos_personales]
dataset[, campo_prueba30 := active_quarter * ccomisiones_otras]

## Bineado de features
#
fwrite(dataset, "./datasets/competencia1_2022.csv")
