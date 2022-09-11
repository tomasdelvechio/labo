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
    #formula = "clase_binaria ~ . - numero_de_cliente",
    formula = "clase_binaria ~ ctrx_quarter___mprestamos_personales + ctrx_quarter___mcuentas_saldo + ctrx_quarter___mactivos_margen + ctrx_quarter___mcaja_ahorro + ctrx_quarter___mcuenta_corriente + ctrx_quarter___ccomisiones_otras + ctrx_quarter___active_quarter + ctrx_quarter___cprestamos_personales + ctrx_quarter___mcomisiones_mantenimiento + ctrx_quarter___mrentabilidad + ctrx_quarter___Visa_status + ctrx_quarter___mpasivos_margen + ctrx_quarter___cdescubierto_preacordado + ctrx_quarter___internet + ctrx_quarter___ccomisiones_mantenimiento + mprestamos_personales___ctrx_quarter + mprestamos_personales___mcuentas_saldo + mprestamos_personales___mactivos_margen + mprestamos_personales___mcaja_ahorro + mprestamos_personales___mcuenta_corriente + mprestamos_personales___ccomisiones_otras + mprestamos_personales___active_quarter + mprestamos_personales___cprestamos_personales + mprestamos_personales___mcomisiones_mantenimiento + mprestamos_personales___mrentabilidad + mprestamos_personales___Visa_status + mprestamos_personales___mpasivos_margen + mprestamos_personales___cdescubierto_preacordado + mprestamos_personales___internet + mprestamos_personales___ccomisiones_mantenimiento + mcuentas_saldo___ctrx_quarter + mcuentas_saldo___mprestamos_personales + mcuentas_saldo___mactivos_margen + mcuentas_saldo___mcaja_ahorro + mcuentas_saldo___mcuenta_corriente + mcuentas_saldo___ccomisiones_otras + mcuentas_saldo___active_quarter + mcuentas_saldo___cprestamos_personales + mcuentas_saldo___mcomisiones_mantenimiento + mcuentas_saldo___mrentabilidad + mcuentas_saldo___Visa_status + mcuentas_saldo___mpasivos_margen + mcuentas_saldo___cdescubierto_preacordado + mcuentas_saldo___internet + mcuentas_saldo___ccomisiones_mantenimiento + mactivos_margen___ctrx_quarter + mactivos_margen___mprestamos_personales + mactivos_margen___mcuentas_saldo + mactivos_margen___mcaja_ahorro + mactivos_margen___mcuenta_corriente + mactivos_margen___ccomisiones_otras + mactivos_margen___active_quarter + mactivos_margen___cprestamos_personales + mactivos_margen___mcomisiones_mantenimiento + mactivos_margen___mrentabilidad + mactivos_margen___Visa_status + mactivos_margen___mpasivos_margen + mactivos_margen___cdescubierto_preacordado + mactivos_margen___internet + mactivos_margen___ccomisiones_mantenimiento + mcaja_ahorro___ctrx_quarter + mcaja_ahorro___mprestamos_personales + mcaja_ahorro___mcuentas_saldo + mcaja_ahorro___mactivos_margen + mcaja_ahorro___mcuenta_corriente + mcaja_ahorro___ccomisiones_otras + mcaja_ahorro___active_quarter + mcaja_ahorro___cprestamos_personales + mcaja_ahorro___mcomisiones_mantenimiento + mcaja_ahorro___mrentabilidad + mcaja_ahorro___Visa_status + mcaja_ahorro___mpasivos_margen + mcaja_ahorro___cdescubierto_preacordado + mcaja_ahorro___internet + mcaja_ahorro___ccomisiones_mantenimiento + mcuenta_corriente___ctrx_quarter + mcuenta_corriente___mprestamos_personales + mcuenta_corriente___mcuentas_saldo + mcuenta_corriente___mactivos_margen + mcuenta_corriente___mcaja_ahorro + mcuenta_corriente___ccomisiones_otras + mcuenta_corriente___active_quarter + mcuenta_corriente___cprestamos_personales + mcuenta_corriente___mcomisiones_mantenimiento + mcuenta_corriente___mrentabilidad + mcuenta_corriente___Visa_status + mcuenta_corriente___mpasivos_margen + mcuenta_corriente___cdescubierto_preacordado + mcuenta_corriente___internet + mcuenta_corriente___ccomisiones_mantenimiento + ccomisiones_otras___ctrx_quarter + ccomisiones_otras___mprestamos_personales + ccomisiones_otras___mcuentas_saldo + ccomisiones_otras___mactivos_margen + ccomisiones_otras___mcaja_ahorro + ccomisiones_otras___mcuenta_corriente + ccomisiones_otras___active_quarter + ccomisiones_otras___cprestamos_personales + ccomisiones_otras___mcomisiones_mantenimiento + ccomisiones_otras___mrentabilidad + ccomisiones_otras___Visa_status + ccomisiones_otras___mpasivos_margen + ccomisiones_otras___cdescubierto_preacordado + ccomisiones_otras___internet + ccomisiones_otras___ccomisiones_mantenimiento + active_quarter___ctrx_quarter + active_quarter___mprestamos_personales + active_quarter___mcuentas_saldo + active_quarter___mactivos_margen + active_quarter___mcaja_ahorro + active_quarter___mcuenta_corriente + active_quarter___ccomisiones_otras + active_quarter___cprestamos_personales + active_quarter___mcomisiones_mantenimiento + active_quarter___mrentabilidad + active_quarter___Visa_status + active_quarter___mpasivos_margen + active_quarter___cdescubierto_preacordado + active_quarter___internet + active_quarter___ccomisiones_mantenimiento + cprestamos_personales___ctrx_quarter + cprestamos_personales___mprestamos_personales + cprestamos_personales___mcuentas_saldo + cprestamos_personales___mactivos_margen + cprestamos_personales___mcaja_ahorro + cprestamos_personales___mcuenta_corriente + cprestamos_personales___ccomisiones_otras + cprestamos_personales___active_quarter + cprestamos_personales___mcomisiones_mantenimiento + cprestamos_personales___mrentabilidad + cprestamos_personales___Visa_status + cprestamos_personales___mpasivos_margen + cprestamos_personales___cdescubierto_preacordado + cprestamos_personales___internet + cprestamos_personales___ccomisiones_mantenimiento + mcomisiones_mantenimiento___ctrx_quarter + mcomisiones_mantenimiento___mprestamos_personales + mcomisiones_mantenimiento___mcuentas_saldo + mcomisiones_mantenimiento___mactivos_margen + mcomisiones_mantenimiento___mcaja_ahorro + mcomisiones_mantenimiento___mcuenta_corriente + mcomisiones_mantenimiento___ccomisiones_otras + mcomisiones_mantenimiento___active_quarter + mcomisiones_mantenimiento___cprestamos_personales + mcomisiones_mantenimiento___mrentabilidad + mcomisiones_mantenimiento___Visa_status + mcomisiones_mantenimiento___mpasivos_margen + mcomisiones_mantenimiento___cdescubierto_preacordado + mcomisiones_mantenimiento___internet + mcomisiones_mantenimiento___ccomisiones_mantenimiento + mrentabilidad___ctrx_quarter + mrentabilidad___mprestamos_personales + mrentabilidad___mcuentas_saldo + mrentabilidad___mactivos_margen + mrentabilidad___mcaja_ahorro + mrentabilidad___mcuenta_corriente + mrentabilidad___ccomisiones_otras + mrentabilidad___active_quarter + mrentabilidad___cprestamos_personales + mrentabilidad___mcomisiones_mantenimiento + mrentabilidad___Visa_status + mrentabilidad___mpasivos_margen + mrentabilidad___cdescubierto_preacordado + mrentabilidad___internet + mrentabilidad___ccomisiones_mantenimiento + Visa_status___ctrx_quarter + Visa_status___mprestamos_personales + Visa_status___mcuentas_saldo + Visa_status___mactivos_margen + Visa_status___mcaja_ahorro + Visa_status___mcuenta_corriente + Visa_status___ccomisiones_otras + Visa_status___active_quarter + Visa_status___cprestamos_personales + Visa_status___mcomisiones_mantenimiento + Visa_status___mrentabilidad + Visa_status___mpasivos_margen + Visa_status___cdescubierto_preacordado + Visa_status___internet + Visa_status___ccomisiones_mantenimiento + mpasivos_margen___ctrx_quarter + mpasivos_margen___mprestamos_personales + mpasivos_margen___mcuentas_saldo + mpasivos_margen___mactivos_margen + mpasivos_margen___mcaja_ahorro + mpasivos_margen___mcuenta_corriente + mpasivos_margen___ccomisiones_otras + mpasivos_margen___active_quarter + mpasivos_margen___cprestamos_personales + mpasivos_margen___mcomisiones_mantenimiento + mpasivos_margen___mrentabilidad + mpasivos_margen___Visa_status + mpasivos_margen___cdescubierto_preacordado + mpasivos_margen___internet + mpasivos_margen___ccomisiones_mantenimiento + cdescubierto_preacordado___ctrx_quarter + cdescubierto_preacordado___mprestamos_personales + cdescubierto_preacordado___mcuentas_saldo + cdescubierto_preacordado___mactivos_margen + cdescubierto_preacordado___mcaja_ahorro + cdescubierto_preacordado___mcuenta_corriente + cdescubierto_preacordado___ccomisiones_otras + cdescubierto_preacordado___active_quarter + cdescubierto_preacordado___cprestamos_personales + cdescubierto_preacordado___mcomisiones_mantenimiento + cdescubierto_preacordado___mrentabilidad + cdescubierto_preacordado___Visa_status + cdescubierto_preacordado___mpasivos_margen + cdescubierto_preacordado___internet + cdescubierto_preacordado___ccomisiones_mantenimiento + internet___ctrx_quarter + internet___mprestamos_personales + internet___mcuentas_saldo + internet___mactivos_margen + internet___mcaja_ahorro + internet___mcuenta_corriente + internet___ccomisiones_otras + internet___active_quarter + internet___cprestamos_personales + internet___mcomisiones_mantenimiento + internet___mrentabilidad + internet___Visa_status + internet___mpasivos_margen + internet___cdescubierto_preacordado + internet___ccomisiones_mantenimiento + ccomisiones_mantenimiento___ctrx_quarter + ccomisiones_mantenimiento___mprestamos_personales + ccomisiones_mantenimiento___mcuentas_saldo + ccomisiones_mantenimiento___mactivos_margen + ccomisiones_mantenimiento___mcaja_ahorro + ccomisiones_mantenimiento___mcuenta_corriente + ccomisiones_mantenimiento___ccomisiones_otras + ccomisiones_mantenimiento___active_quarter + ccomisiones_mantenimiento___cprestamos_personales + ccomisiones_mantenimiento___mcomisiones_mantenimiento + ccomisiones_mantenimiento___mrentabilidad + ccomisiones_mantenimiento___Visa_status + ccomisiones_mantenimiento___mpasivos_margen + ccomisiones_mantenimiento___cdescubierto_preacordado + ccomisiones_mantenimiento___internet + ctrx_quarter + mprestamos_personales + mcuentas_saldo + mactivos_margen + mcaja_ahorro + mcuenta_corriente + ccomisiones_otras + active_quarter + cprestamos_personales + mcomisiones_mantenimiento + mrentabilidad + Visa_status + mpasivos_margen + cdescubierto_preacordado + internet + ccomisiones_mantenimiento - numero_de_cliente",
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