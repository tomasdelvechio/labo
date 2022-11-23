# Compara dos envios, indicando clientes que coinciden en ambos y
#   clientes que estan presentes en uno y no en otro.
# Se toma de base la prediccion == 1

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()

# Se listan INDIVIDUALMENTE los semillerios que se desean incluir
# Estos archivos los genera el z1292 cuando tiene PARAM$generar_salida_hibridador <- TRUE
#PARAM$archivo_uno <- "../ZZ1292_hibridacion_semillerio_kaggle_m1/ZZ1292_hibridacion_semillerio_kaggle_m1_hibridacion_C10250.csv"
PARAM$archivo_uno <- "../ZZ1292_semillerio_kaggle_m1/ZZ1292_semillerio_kaggle_m1_ensamble_rank_C10250.csv"
#PARAM$archivo_dos <- "../ZZ1292_semillerio_kaggle_m1/ZZ1292_semillerio_kaggle_m1_ensamble_rank_C10250.csv"
#PARAM$archivo_dos <- "../ZZ1292_semillerio_kaggle_m3/ZZ1292_semillerio_kaggle_m3_ensamble_rank_C10250.csv"
PARAM$archivo_dos <- "../ZZ1292_semillerio_kaggle_m2/ZZ1292_semillerio_kaggle_m2_ensamble_rank_C10250.csv"

# FIN Parametros del script

options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

tb_pred_uno <- fread(PARAM$archivo_uno)
tb_pred_dos <- fread(PARAM$archivo_dos)

if(sum(dim(tb_pred_dos) == dim(tb_pred_uno)) != 2) {
    cat("Los envios no son comparables")
    quit(1)
}

setorder(tb_pred_uno, numero_de_cliente)
setorder(tb_pred_dos, numero_de_cliente)

tb_comparacion <- tb_pred_uno
colnames(tb_comparacion) <- c("numero_de_cliente", "Predicted_uno")

tb_comparacion[, Predicted_dos := tb_pred_dos$Predicted]

tb_comparacion[, Diff := Predicted_uno - Predicted_dos]

tb_comparacion[Diff == 0, .N]  # Los que se predijeron iguales en ambos, sean 1 o 0
tb_comparacion[Diff == 1, .N]  # Los que se predijeron 1 en el archivo 1 pero 0 en el segundo
tb_comparacion[Diff == -1, .N] # Los que se predijeron 0 en el 1er archivo pero 1 en el segundo
