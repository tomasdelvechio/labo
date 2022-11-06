# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ganancias_semillerio"
PARAM$exp_input <- "ZZ9410_semillerio"
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

# Cargar las semillas usadas para levantar las ganancias en el orden que fueron calculadas
arch_future <- paste0(base_dir, "exp/", PARAM$exp_input, "/ksemillas.csv")
ksemillas <- read.csv(arch_future, header = TRUE)$x

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
dataset <- fread(arch_dataset)

dataset_julio <- dataset[foto_mes == 202107]
rm(dataset)

dataset_julio[, clase_real := ifelse(clase_ternaria == "BAJA+2", 1, 0)]
# Nos quedamos con las 2 columnas que nos resultan relevantes
dataset_julio <- dataset_julio[, .("numero_de_cliente", "clase_real")]

calcularGanancia <- function(real, predicho) {
    tb_comparacion <- merge(real, predicho)
    # Estoy seguro que tiene que existir una forma menos horrible de escribir la siguiente expresión
    return (tb_comparacion[, sum(ifelse(real == 1 & predicho == 1, 78000, ifelse(real == 0 & predicho == 1, -2000, 0)))])
}

tb_ganancias <- data.table(ksemillas)
tb_ganancias[, individual := 0]
tb_ganancias[, semillerio := 0]

pdf("semillerio_vs_individuales.pdf")

for (ksemilla in ksemillas) {
    arch_prediccion_individual <- paste0(
        base_dir, "exp/", PARAM$exp_input, "/", PARAM$exp_input, "_", sprintf("%d", ksemilla), ".csv"
    )
    tb_prediccion_individual <- fread(arch_prediccion_individual)

    arch_prediccion_semillerio <- paste0(
        base_dir, "exp/", PARAM$exp_input, "/", PARAM$exp_input, "_", sprintf("%d", ksemilla), "_semillerio.csv"
    )
    tb_prediccion_semillerio <- fread(arch_prediccion_semillerio)

    ganancia_individual <- calcularGanancia(dataset_julio, tb_prediccion_individual)
    ganancia_semillerio <- calcularGanancia(dataset_julio, arch_prediccion_semillerio)

    tb_ganancias["semilla" == ksemilla]$individual <- calcularGanancia(dataset_julio, tb_prediccion_individual)
    tb_ganancias["semilla" == ksemilla]$semillerio <- calcularGanancia(dataset_julio, tb_prediccion_individual)

}

plot(tb_ganancias$semilla, tb_ganancias$semillerio, type = "l", col = "red")
points(tb_ganancias$semilla, tb_ganancias$individual, col = "blue")
lines(mean(tb_ganancias$individual), color = "green")


dev.off()