# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("randomForest")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "CLU1270_v1"
PARAM$exp_input <- "CLU1262_v1" # Uso mi mejor Dataset de la C3
# FIN Parametros del script

setwd("~/buckets/b1/") # cambiar por la carpeta local

campos_buenos <- c(
    "ctrx_quarter_normalizado", "ctrx_quarter", "ctrx_quarter_lag1",
    "mcuentas_saldo_rank", "ctrx_quarter_normalizado_lag2", "mcaja_ahorro_rank",
    "mpayroll_sobre_edad_rank", "ctrx_quarter_normalizado_lag1", "cpayroll_trx",
    "mprestamos_personales_rank", "mtarjeta_visa_consumo_rank", "mpayroll_rank",
    "mpasivos_margen_rank", "ctrx_quarter_lag2", "ctrx_quarter_normalizado_avg6"
)

campos_originales_de_interes <- c( "cliente_vip", "internet", "cliente_edad", "cliente_antiguedad", "mrentabilidad",
  "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen",
  "cproductos", "tcuentas", "ccuenta_corriente", "mcuenta_corriente_adicional",
  "mcuenta_corriente", "ccaja_ahorro", "mcaja_ahorro", "mcaja_ahorro_adicional",
  "mcaja_ahorro_dolares", "cdescubierto_preacordado", "mcuentas_saldo", "ctarjeta_debito",
  "ctarjeta_debito_transacciones", "mautoservicio", "ctarjeta_visa",
  "ctarjeta_visa_transacciones", "mtarjeta_visa_consumo", "ctarjeta_master",
  "ctarjeta_master_transacciones", "mtarjeta_master_consumo", "cprestamos_personales",
  "mprestamos_personales", "cprestamos_prendarios", "mprestamos_prendarios",
  "cprestamos_hipotecarios", "mprestamos_hipotecarios", "cplazo_fijo",
  "mplazo_fijo_dolares", "mplazo_fijo_pesos", "cinversion1", "minversion1_pesos",
  "minversion1_dolares", "cinversion2", "minversion2"
)

# cargo el dataset donde voy a entrenar
dataset_input <- paste0("./exp/", PARAM$exp_input, "/cluster_de_bajas_12meses.txt")
dataset <- fread(dataset_input)
dataset[, clase_ternaria := NULL] # no sirve para nada, es todo BAJA+2

dataset_original_input <- paste0("./exp/", PARAM$exp_input, "/dataset_original_bajas_mas_dos.csv")
dataset_original <- fread(dataset_original_input)

dataset <- cbind(dataset, dataset_original)
dataset <- dataset[, clase_ternaria := NULL]

# Algunas exploraciones
dataset[, length(numero_de_cliente), cluster2] # cantidad de miembros por cluster
dataset <- na.roughfix(dataset)

## COMIENZA EL ANALISIS
distribucion_clientes_cluster <- dataset[, length(numero_de_cliente), by = c("cluster2", "foto_mes")]
distribucion_clientes_cluster[, foto_mes := as.character(foto_mes)]
ggplot(data = distribucion_clientes_cluster, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()

cajas_ahorro <- dataset[, sum(mcaja_ahorro), by = c("cluster2", "foto_mes")]
cajas_ahorro[, foto_mes := as.character(foto_mes)]
ggplot(data = cajas_ahorro, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    theme_classic()


inversiones <- dataset[, sum(mcaja_ahorro), by = c("cluster2", "foto_mes")]
inversiones[, foto_mes := as.character(foto_mes)]
ggplot(data = inversiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    theme_classic()

#saldos <- dataset[, sum(mcuentas_saldo), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(mcaja_ahorro), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(ctrx_quarter), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(cpayroll_trx), by = c("cluster2", "foto_mes")]
saldos <- dataset[, sum(cpayroll_trx), by = c("cluster2", "foto_mes")]
saldos[, foto_mes := as.character(foto_mes)]

require("ggplot2")
require("tidyr")

#plot(dataset[,cluster2==1]$foto_mes, dataset[,cluster2==1]$V1)
ggplot(data = saldos, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point()
    + theme_minimal()




dataset[, mean(ctrx_quarter), cluster2] # media de la variable  ctrx_quarter
dataset[, mean(ctrx_quarter_normalizado), cluster2] # media de la variable  ctrx_quarter
dataset[, mean(ctrx_quarter_lag1), cluster2] # media de la variable  ctrx_quarter
dataset[, mean(ctrx_quarter_normalizado_lag2), cluster2] # media de la variable  ctrx_quarter
dataset[, mean(ctrx_quarter_lag2), cluster2] # media de la variable  ctrx_quarter
dataset[, mean(ctrx_quarter_normalizado_avg6), cluster2] # media de la variable  ctrx_quarter

dataset[, mean(mcuentas_saldo_rank), cluster2]

dataset[, mean(mcaja_ahorro_rank), cluster2]

dataset[, mean(mpayroll_sobre_edad_rank), cluster2]
dataset[, mean(mpayroll_rank), cluster2]

dataset[, mean(cpayroll_trx), cluster2]

dataset[, mean(mprestamos_personales_rank), cluster2]

dataset[, mean(mtarjeta_visa_consumo_rank), cluster2]

dataset[, mean(mpasivos_margen_rank), cluster2]

tendencia_bajas <- dataset[, length(numero_de_cliente), foto_mes]
setorder(tendencia_bajas, foto_mes)
tendencia_bajas
