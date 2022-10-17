
a <<- 3

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/tomas/workspace/uba/dmeyf")
# Poner sus semillas
semillas <- c(697157, 585799, 906007, 748301, 372871)

# Cargamos el dataset
dataset <- fread("./datasets/competencia3_2022.csv.gz")
