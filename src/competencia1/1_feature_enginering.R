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

# Arma secreta aportada por Gustavo Denicolay
dataset[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
dataset[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]

dataset[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
dataset[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]

dataset[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
dataset[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]

dataset[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
dataset[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

fwrite(dataset, "./datasets/competencia1_2022.csv")
