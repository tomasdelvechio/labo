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
# Poner sus semillas
# semillas <- c(17, 19, 23, 29, 31)
semillas <- c(697157, 585799, 906007, 748301, 372871)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

parameters <- read.csv("./datasets/rpart_parameters_ob2_1.csv", sep = ";")

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

        modelo <- rpart(clase_binaria ~ . - numero_de_cliente,
            data = dtrain,
            xval = parameters$xval,
            cp = parameters$cp,
            minsplit = parameters$minsplit,
            minbucket = parameters$minbucket,
            maxdepth = parameters$maxdepth
        )
        pred_testing <- predict(modelo, dtest, type = "prob")

        cat(ganancia(pred_testing[, "evento"], dtest$clase_binaria, punto_corte) / 0.3, semilla, "\n")
    }
}

rendimiento_puntos_corte_manual <- function(df, semillas) {
    rango_puntos_corte <- seq(24, 40) / 1000
    for (punto_corte in rango_puntos_corte) {
        cat("Punto de corte", punto_corte, "\n")
        rendimiento_semillas_training(dtrain, semillas, punto_corte = punto_corte)
    }

}

#rendimiento_semillas_training(dtrain, semillas)
#rendimiento_puntos_corte_manual(dtrain, semillas)

modelo <- rpart(
    formula = "clase_binaria ~ . - numero_de_cliente",
    data = dtrain,
    xval = parameters$xval,
    cp = parameters$cp,
    minsplit = parameters$minsplit,
    minbucket = parameters$minbucket,
    maxdepth = parameters$maxdepth
)

#prp(modelo, extra = 101, digits = 5, branch = 1, type = 4, varlen = 0, faclen = 0)

prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
)

punto_de_corte <- 0.047

dapply[, prob_baja2 := prediccion[, "evento"]]
dapply[, Predicted := as.numeric(prob_baja2 > punto_de_corte)]

#cat(ganancia(dapply$prob_baja2, dapply$clase_binaria), semilla, "\n")

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KAGC1", showWarnings = FALSE)

# Crea un nombre de archivo unico
output_filename <- paste0("output-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", round(runif(1) * 100), ".csv")

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
    file = paste0("./exp/KAGC1/", output_filename),
    sep = ","
)

cat("Punto de Corte:", punto_de_corte, "\n")
cat("Cantidad de Bajas Predichas:", sum(dapply$Predicted), "\n")
cat("Resultados para subir a Kaggle:", output_filename, "\n")