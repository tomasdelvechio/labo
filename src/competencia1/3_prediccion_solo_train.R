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

parameters <- read.csv("./datasets/rpart_parameters_ob2_1.csv", sep = ";")

dtrain <- dataset[foto_mes == 202101] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202103] # defino donde voy a aplicar el modelo

ganancia <- function(probabilidades, clase, punto_corte = 0.025) {
    return(sum(
        (probabilidades >= punto_corte) * ifelse(clase == "evento", 78000, -2000))
    )
}

rendimiento_semillas_training <- function(df, semillas, p = 0.70, punto_corte = 0.025) {
    #cat("Probando Semillas en Train\n")
    #formulas <- list(
        #"clase_binaria ~ . - ctrx_quarter",
        #"clase_binaria ~ .",
        #"clase_binaria ~ - numero_de_cliente + campo1 + ctrx_quarter"
    #)
    variables_disponibles <- c(
        "campo1",
        "ctrx_quarter",
        "active_quarter___ctrx_quarter",
        "campo_prueba26",
        "campo_prueba5",
        "ctrx_quarter___active_quarter",
        "r_ctrx_quarter",
        "cdescubierto_preacordado___mcuentas_saldo",
        "mcuentas_saldo___cdescubierto_preacordado",
        "mcuentas_saldo",
        "cdescubierto_preacordado___mcuenta_corriente",
        "mcuenta_corriente___cdescubierto_preacordado",
        "active_quarter___Visa_status",
        "Visa_status___active_quarter",
        "cliente_antiguedad",
        "Visa_status",
        "campo5",
        "cdescubierto_preacordado___Visa_status",
        "Visa_status___cdescubierto_preacordado",
        "cliente_edad",
        "cdescubierto_preacordado",
        "mpasivos_margen",
        "mpasivos_margen___cdescubierto_preacordado",
        "Master_fechaalta",
        "mcuentas_saldo___mpasivos_margen",
        "mcuenta_corriente___mpasivos_margen",
        "mpasivos_margen___mcuenta_corriente",
        "mcomisiones",
        "mcomisiones_otras",
        "cdescubierto_preacordado___mpasivos_margen",
        "mrentabilidad_annual",
        "mcomisiones_mantenimiento",
        "ccomisiones_otras___mcomisiones_mantenimiento",
        "mrentabilidad___cdescubierto_preacordado",
        "mrentabilidad",
        "mprestamos_personales",
        "mcomisiones_mantenimiento___ccomisiones_otras",
        "mactivos_margen",
        "ctrx_quarter___mpasivos_margen",
        "campo_prueba20",
        "mpasivos_margen___mcuentas_saldo",
        "mcuenta_corriente___ccomisiones_mantenimiento",
        "campo_prueba10",
        "internet",
        "ctarjeta_master",
        "active_quarter___mpasivos_margen",
        "mpasivos_margen___active_quarter",
        "mcuenta_corriente___internet",
        "ccomisiones_otras",
        "internet___mcuenta_corriente",
        "campo_prueba12",
        "campo_prueba7",
        "r_mprestamos_personales",
        "cproductos",
        "ccomisiones_mantenimiento___mcuenta_corriente",
        "Visa_msaldopesos",
        "mactivos_margen___internet",
        "Master_Fvencimiento",
        "mcuenta_corriente",
        "mactivos_margen___mpasivos_margen",
        "mpasivos_margen___mactivos_margen",
        "r_ccomisiones_mantenimiento",
        "internet___mactivos_margen",
        "Visa_msaldototal",
        "cprestamos_personales___mprestamos_personales",
        "mprestamos_personales___cprestamos_personales",
        "campo_prueba29",
        "mcomisiones_mantenimiento___mcuentas_saldo",
        "mcuentas_saldo___mcomisiones_mantenimiento",
        "ccomisiones_mantenimiento___mcuentas_saldo",
        "mcuentas_saldo___ccomisiones_mantenimiento",
        "mcomisiones_mantenimiento___ccomisiones_mantenimiento",
        "internet___mcuentas_saldo",
        "mcuentas_saldo___internet",
        "mtarjeta_visa_consumo",
        "r_mcomisiones_mantenimiento",
        "campo_prueba27",
        "campo_prueba22",
        "campo_prueba9",
        "ctarjeta_visa",
        "r_internet",
        "mactivos_margen___mcomisiones_mantenimiento",
        "ctarjeta_master_debitos_automaticos",
        "mttarjeta_master_debitos_automaticos",
        "ctrx_quarter___mactivos_margen",
        "matm",
        "campo_prueba1",
        "mactivos_margen___mrentabilidad",
        "mrentabilidad___mactivos_margen",
        "mcuentas_saldo___mactivos_margen",
        "ctarjeta_visa_debitos_automaticos",
        "mcaja_ahorro_dolares",
        "mttarjeta_visa_debitos_automaticos",
        "ccaja_ahorro",
        "Visa_fechaalta",
        "campo_prueba16",
        "campo_prueba3",
        "ctrx_quarter___mprestamos_personales",
        "mprestamos_personales___ctrx_quarter",
        "mpayroll",
        "cpayroll_trx",
        "chomebanking_transacciones",
        "mactivos_margen___mcaja_ahorro",
        "ccallcenter_transacciones",
        "tcallcenter",
        "active_quarter___mcaja_ahorro",
        "mcaja_ahorro",
        "mcaja_ahorro___active_quarter",
        "mcaja_ahorro___cdescubierto_preacordado",
        "mcaja_ahorro___mpasivos_margen",
        "mpasivos_margen___mcaja_ahorro",
        "campo_prueba14",
        "internet___mrentabilidad",
        "mrentabilidad___internet",
        "catm_trx_other",
        "mcomisiones_mantenimiento___mactivos_margen",
        "ctrx_quarter___mrentabilidad",
        "mcuentas_saldo___mrentabilidad",
        "mrentabilidad___ctrx_quarter",
        "mrentabilidad___mcuentas_saldo",
        "catm_trx",
        "mactivos_margen___mcuenta_corriente",
        "mcuenta_corriente___mactivos_margen",
        "campo_prueba6",
        "mcuentas_saldo___Visa_status",
        "ccomisiones_mantenimiento___mcomisiones_mantenimiento",
        "mcaja_ahorro___mcuentas_saldo",
        "mcuentas_saldo___mcaja_ahorro",
        "active_quarter___mcomisiones_mantenimiento",
        "mcomisiones_mantenimiento___active_quarter",
        "cdescubierto_preacordado___cprestamos_personales",
        "cdescubierto_preacordado___mprestamos_personales",
        "cprestamos_personales___cdescubierto_preacordado",
        "mprestamos_personales___cdescubierto_preacordado",
        "matm_other",
        "mactivos_margen___ccomisiones_mantenimiento",
        "mcaja_ahorro___mrentabilidad",
        "mrentabilidad___mcaja_ahorro",
        "ctarjeta_debito",
        "ctarjeta_visa_transacciones",
        "active_quarter___ccomisiones_mantenimiento",
        "ccomisiones_mantenimiento",
        "mcuentas_saldo___active_quarter",
        "campo_prueba2",
        "campo_prueba4",
        "Master_mpagospesos",
        "mpasivos_margen___ctrx_quarter",
        "ctrx_quarter___cdescubierto_preacordado",
        "mcaja_ahorro___mcomisiones_mantenimiento",
        "mcomisiones_mantenimiento___mcaja_ahorro",
        "Visa_mpagominimo",
        "mcaja_ahorro___mactivos_margen",
        "r_cprestamos_personales",
        "ccajas_consultas",
        "r_mactivos_margen",
        "ccomisiones_mantenimiento___active_quarter",
        "tmobile_app",
        "Visa_Fvencimiento",
        "cd_v2_1",
        "mprestamos_personales___mactivos_margen",
        "mprestamos_personales___mcomisiones_mantenimiento",
        "ccomisiones_mantenimiento___mcaja_ahorro",
        "mcaja_ahorro___ccomisiones_mantenimiento",
        "mtransferencias_recibidas",
        "mextraccion_autoservicio",
        "Visa_status___mpasivos_margen",
        "mpasivos_margen___internet",
        "cinversion2",
        "mrentabilidad___Visa_status",
        "mcaja_ahorro___cprestamos_personales",
        "mcuenta_corriente___cprestamos_personales",
        "mcheques_depositados",
        "cseguro_auto",
        "cdescubierto_preacordado___ccomisiones_mantenimiento",
        "mpasivos_margen___mrentabilidad",
        "mrentabilidad___mpasivos_margen",
        "Master_mfinanciacion_limite",
        "ctarjeta_debito_transacciones",
        "mcaja_ahorro_adicional",
        "r_cdescubierto_preacordado",
        "active_quarter___mrentabilidad",
        "mrentabilidad___active_quarter",
        "r_mrentabilidad",
        "mrentabilidad___ccomisiones_mantenimiento",
        "ctrx_quarter___ccomisiones_mantenimiento",
        "Visa_mconsumosdolares",
        "mcomisiones_mantenimiento___mcuenta_corriente",
        "mcuenta_corriente___mcomisiones_mantenimiento",
        "mcuenta_corriente___mcuentas_saldo",
        "mcuentas_saldo___mcuenta_corriente",
        "ccomisiones_otras___cdescubierto_preacordado",
        "ctrx_quarter___mcomisiones_mantenimiento",
        "r_ccomisiones_otras",
        "cprestamos_personales___mcuentas_saldo",
        "mcuentas_saldo___cprestamos_personales",
        "internet___mcaja_ahorro",
        "mcaja_ahorro___internet",
        "Visa_mfinanciacion_limite",
        "Visa_mlimitecompra",
        "Visa_mconsumospesos",
        "Visa_mconsumototal",
        "cextraccion_autoservicio",
        "cd_v2_5",
        "r_active_quarter",
        "Visa_mpagospesos",
        "ctransferencias_recibidas",
        "Master_mlimitecompra",
        "cdescubierto_preacordado___mcomisiones_mantenimiento",
        "mcomisiones_mantenimiento___cdescubierto_preacordado",
        "mprestamos_prendarios",
        "internet___mpasivos_margen"
    )

    variables_en_uso <- c()

    for (variable in variables_disponibles) {
        variables_en_uso <- c(variables_en_uso, variable)
        # print(paste0("clase_binaria ~ ", paste(variables_en_uso, collapse = " + ")))
        # print("\n")

        formula <- paste0("clase_binaria ~ ", paste(variables_en_uso, collapse = " + "))

        for (semilla in semillas) {
            # Seteamos nuestra primera semilla
            set.seed(semilla)

            # Particionamos de forma estratificada
            in_training <- caret::createDataPartition(df$clase_binaria,
                p = p, list = FALSE
            )
            dtrain <- df[in_training, ]
            dtest <- df[-in_training, ]

            modelo <- rpart(formula,
                data = dtrain,
                xval = parameters$xval,
                cp = parameters$cp,
                minsplit = parameters$minsplit,
                minbucket = parameters$minbucket,
                maxdepth = parameters$maxdepth
            )
            pred_testing <- predict(modelo, dtest, type = "prob")

            cat(punto_corte, semilla, formula, ganancia(pred_testing[, "evento"], dtest$clase_binaria, punto_corte) / 0.3, "\n", sep = ";")
            #prp(modelo, extra = 101, digits = 5, branch = 1, type = 4, varlen = 0, faclen = 0)
        }
    }
}

rendimiento_puntos_corte_manual <- function(df, semillas) {
    rango_puntos_corte <- seq(25, 59, 4) / 1000
    for (punto_corte in rango_puntos_corte) {
        #cat("Punto de corte", punto_corte, "\n")
        rendimiento_semillas_training(dtrain, semillas, punto_corte = punto_corte)
    }
}

sink("outfile.txt")
cat("Punto de Corte", "Semilla", "Formula", "Ganancia", "\n", sep = ";")

rendimiento_semillas_training(dtrain, semillas, punto_corte = 0.0291)
#rendimiento_puntos_corte_manual(dtrain, semillas)
sink()