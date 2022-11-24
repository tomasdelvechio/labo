# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1297_stacking_semillerio_kaggle"

# Se listan INDIVIDUALMENTE los semillerios que se desean incluir
# Estos archivos los genera el z1292 cuando tiene PARAM$generar_salida_hibridador <- TRUE
PARAM$archivos <- c(
    "../ZZ1292_semillerio_kaggle_m1/ZZ1292_semillerio_kaggle_m1_ensamble_rank_C10250_4.csv",
    "../ZZ1292_semillerio_kaggle_m2/ZZ1292_semillerio_kaggle_m2_ensamble_rank_C10250.csv",
    "../ZZ1292_semillerio_kaggle_m3/ZZ1292_semillerio_kaggle_m3_ensamble_rank_C10250.csv"
)

PARAM$corte <- 10250 # cantidad de envios
# FIN Parametros del script

options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

cat("Semillerios involucrados en la hibridacion: ", length(PARAM$archivos), "\n")
cat("Directorio de salida: ", getwd(), "\n")

for (indice_semillerio in seq_along(PARAM$archivos)) {

    # cols: numero_de_cliente,Predicted
    tb_prediccion_semillerio <- fread(PARAM$archivos[indice_semillerio])
    setorder(tb_prediccion_semillerio, numero_de_cliente)

    if(!exists("tb_stacking")) {
        tb_stacking <- data.table(numero_de_cliente = tb_prediccion_semillerio[, numero_de_cliente])
    }

    tb_stacking[, paste0("prediccion_", indice_semillerio) := tb_prediccion_semillerio$Predicted]
}

tb_stacking[, Stack := NULL]
tb_stacking[, Stack := (rowSums(tb_stacking) - numero_de_cliente)]
setorder(tb_stacking, -Stack)
corte <- tb_stacking[Stack > 1, .N]

tb_stacking[, Predicted := 0]
tb_stacking[1:corte, Predicted := 1L]

nombre_arch_ensamble <- paste0(
    PARAM$experimento,
    "_",
    "stacking",
    "_",
    sprintf("C%d", corte),
    ".csv"
)
fwrite(
    tb_stacking[, list(numero_de_cliente, Predicted)],
    file = nombre_arch_ensamble,
    sep = ","
)
