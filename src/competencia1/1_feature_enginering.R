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

fwrite(dataset, "./datasets/competencia1_2022.csv")
