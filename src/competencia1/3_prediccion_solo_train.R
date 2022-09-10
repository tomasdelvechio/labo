rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("rpart.plot")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("rgenoud")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/tomas/workspace/uba/dmeyf")

semillas <- c(697157, 585799, 906007, 748301, 372871)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

parameters <- read.csv("./datasets/rpart_parameters.csv", sep = ";")

dtrain <- dataset[foto_mes == 202101] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202103] # defino donde voy a aplicar el modelo

ganancia <- function(probabilidades, clase, punto_corte = 0.025) {
    return(sum(
        (probabilidades >= punto_corte) * ifelse(clase == "evento", 78000, -2000))
    )
}

rendimiento_semillas_training <- function(df, semillas, p = 0.70, punto_corte = 0.025) {
    cat("Probando Semillas en Train\n")
    for (semilla in semillas) {
        # Seteamos nuestra primera semilla
        set.seed(semilla)

        # Particionamos de forma estratificada
        in_training <- caret::createDataPartition(df$clase_binaria,
            p = p, list = FALSE
        )
        dtrain <- df[in_training, ]
        dtest <- df[-in_training, ]

        modelo <- rpart(clase_binaria ~ . -ctrx_quarter,
            data = dtrain,
            xval = parameters$xval,
            cp = parameters$cp,
            minsplit = parameters$minsplit,
            minbucket = parameters$minbucket,
            maxdepth = 4
        )
        pred_testing <- predict(modelo, dtest, type = "prob")

        cat(ganancia(pred_testing[, "evento"], dtest$clase_binaria, punto_corte) / 0.3, semilla, "\n")
        prp(modelo, extra = 101, digits = 5, branch = 1, type = 4, varlen = 0, faclen = 0)
    }
}

rendimiento_puntos_corte_manual <- function(df, semillas) {
    rango_puntos_corte = seq(24, 40) / 1000
    for (punto_corte in rango_puntos_corte) {
        cat("Punto de corte", punto_corte, "\n")
        rendimiento_semillas_training(dtrain, semillas, punto_corte = punto_corte)
    }
}

rendimiento_semillas_training(dtrain, semillas)
#rendimiento_puntos_corte_manual(dtrain, semillas)