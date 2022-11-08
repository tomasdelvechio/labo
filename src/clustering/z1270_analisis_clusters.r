# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("randomForest")
require("ggplot2")
require("tidyr")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "CLU1270_v4"
PARAM$exp_input <- "CLU1262_v4" # Uso mi mejor Dataset de la C3
# FIN Parametros del script

setwd("~/buckets/b1/") # cambiar por la carpeta local

#campos_buenos <- c(
#    "ctrx_quarter_normalizado", "ctrx_quarter", "ctrx_quarter_lag1",
#    "mcuentas_saldo_rank", "ctrx_quarter_normalizado_lag2", "mcaja_ahorro_rank",
#    "mpayroll_sobre_edad_rank", "ctrx_quarter_normalizado_lag1", "cpayroll_trx",
#    "mprestamos_personales_rank", "mtarjeta_visa_consumo_rank", "mpayroll_rank",
#    "mpasivos_margen_rank", "ctrx_quarter_lag2", "ctrx_quarter_normalizado_avg6"
#)

#campos_buenos <- c( "cliente_vip", "internet", "cliente_edad", "cliente_antiguedad", "mrentabilidad",
#  "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen",
#  "cproductos", "tcuentas", "ccuenta_corriente", "mcuenta_corriente_adicional",
#  "mcuenta_corriente", "ccaja_ahorro", "mcaja_ahorro", "mcaja_ahorro_adicional",
#  "mcaja_ahorro_dolares", "cdescubierto_preacordado", "mcuentas_saldo", "ctarjeta_debito",
#  "ctarjeta_debito_transacciones", "mautoservicio", "ctarjeta_visa",
#  "ctarjeta_visa_transacciones", "mtarjeta_visa_consumo", "ctarjeta_master",
#  "ctarjeta_master_transacciones", "mtarjeta_master_consumo", "cprestamos_personales",
#  "mprestamos_personales", "cprestamos_prendarios", "mprestamos_prendarios",
#  "cprestamos_hipotecarios", "mprestamos_hipotecarios", "cplazo_fijo",
#  "mplazo_fijo_dolares", "mplazo_fijo_pesos", "cinversion1", "minversion1_pesos",
#  "minversion1_dolares", "cinversion2", "minversion2"
#)

# cargo el dataset donde voy a entrenar
dataset_input <- paste0("./exp/", PARAM$exp_input, "/cluster_de_bajas_12meses.txt")
dataset <- fread(dataset_input)
dataset[, clase_ternaria := NULL] # no sirve para nada, es todo BAJA+2

#dataset_original_input <- paste0("./exp/", PARAM$exp_input, "/dataset_original_bajas_mas_dos.csv")
#dataset_original <- fread(dataset_original_input)

#dataset <- cbind(dataset, dataset_original)
#dataset <- dataset[, clase_ternaria := NULL]

# Clientes por grupo
clusters <- dataset[, length(numero_de_cliente), cluster2] # cantidad de miembros por cluster
dataset <- na.roughfix(dataset)
clusters[, cluster2 := as.character(cluster2)]
ggplot(data = clusters, aes(x = cluster2, y = V1)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(
        title = "Clientes por grupo de analisis",
        x = "Grupos", y = "Clientes"
    ) +
    geom_text(aes(label = V1, y = 0.5 * V1), size = 5) +
    theme_classic()


## Clientes x grupo x mes
distribucion_clientes_cluster <- dataset[, length(numero_de_cliente), by = c("cluster2", "foto_mes")]
distribucion_clientes_cluster[, foto_mes := as.character(foto_mes)]
ggplot(data = distribucion_clientes_cluster, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Tendencia de clientes en baja por mes y grupo",
        x = "Meses", y = "Clientes",
        colour = "Grupos"
    ) +
    theme_classic()

# Ver clientes por cluster por mes
order_cols <- c("foto_mes", "cluster2")
setorderv(distribucion_clientes_cluster, order_cols)
ggplot(distribucion_clientes_cluster, aes(fill = factor(cluster2), y = V1, x = foto_mes)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic()


# Movimientos
movimientos <- dataset[, sum(ctrx_quarter) / length(ctrx_quarter), by = c("cluster2", "foto_mes")]
movimientos[, foto_mes := as.character(foto_mes)]
ggplot(data = movimientos, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Movimientos por grupo de clientes (normalizado)",
        x = "Meses", y = "Movimientos",
        colour = "Grupos"
    ) +
    theme_classic()

# Cobra sueldo
#sueldos <- dataset[cluster2 != 3, sum(cpayroll_trx) / length(cpayroll_trx), by = c("cluster2", "foto_mes")]
sueldos <- dataset[, sum(cpayroll_trx) / length(cpayroll_trx), by = c("cluster2", "foto_mes")]
sueldos[, foto_mes := as.character(foto_mes)]
ggplot(data = sueldos, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Payroll por grupo de clientes (normalizado)",
        x = "Meses", y = "Tasa de Payroll",
        colour = "Grupos"
    ) +
    theme_classic()

# usa homebanking
dataset[, usa_internet := ifelse(internet == 0, 0, 1)]
internet <- dataset[, sum(usa_internet) / length(usa_internet), by = c("cluster2", "foto_mes")]
internet[, foto_mes := as.character(foto_mes)]
ggplot(data = internet, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Uso de HomeBanking por grupo de clientes (normalizado)",
        x = "Meses", y = "Tasa de uso de HomeBanking",
        colour = "Grupos"
    ) +
    theme_classic()


# productos con el banco
#productos <- dataset[, (sum(ccuenta_corriente) + sum(ccaja_ahorro)) / length(ccuenta_corriente), by = c("cluster2", "foto_mes")]
productos <- dataset[, sum(cproductos) / length(cproductos), by = c("cluster2", "foto_mes")]
productos[, foto_mes := as.character(foto_mes)]
ggplot(data = productos, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Familias de Productos adquiridos por grupo de clientes (normalizado)",
        x = "Meses", y = "Tasa de Productos",
        colour = "Grupos"
    ) +
    theme_classic()


# mrentabilidad
# productos <- dataset[, (sum(ccuenta_corriente) + sum(ccaja_ahorro)) / length(ccuenta_corriente), by = c("cluster2", "foto_mes")]
rentabilidad <- dataset[, sum(mrentabilidad) / length(mrentabilidad), by = c("cluster2", "foto_mes")]
rentabilidad[, foto_mes := as.character(foto_mes)]
ggplot(data = rentabilidad, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Rentabilidad promedio por grupo de clientes (normalizado)",
        x = "Meses", y = "Rentabilidad promedio por cliente",
        colour = "Grupos"
    ) +
    theme_classic()


# mcomisiones
comisiones <- dataset[, sum(mcomisiones) / length(mcomisiones), by = c("cluster2", "foto_mes")]
comisiones[, foto_mes := as.character(foto_mes)]
ggplot(data = comisiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Comisiones promedio del grupo de clientes (normalizado)",
        x = "Meses", y = "Comisión promedio por cliente",
        colour = "Grupos"
    ) +
    theme_classic()


# inversiones
#inversiones <- dataset[, (sum(minversion1_pesos) + sum(minversion1_dolares) + sum(minversion2)) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
inversiones <- dataset[, (sum(mplazo_fijo_dolares) + sum(mplazo_fijo_pesos) + sum(minversion2)) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
inversiones[, foto_mes := as.character(foto_mes)]
ggplot(data = inversiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Promedio de monto de Plazos Fijos del grupo de clientes (normalizado)",
        x = "Meses", y = "Promedio de monto de Plazo Fijo",
        colour = "Grupos"
    ) +
    theme_classic()

# Prestamos
prestamos <- dataset[(mprestamos_personales > 0) | (mprestamos_prendarios > 0) | (mprestamos_hipotecarios > 0), (sum(mprestamos_personales) + sum(mprestamos_prendarios) + sum(mprestamos_hipotecarios)) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
#prestamos <- dataset[(cprestamos_personales > 0), (sum(cprestamos_personales)) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
prestamos[, foto_mes := as.character(foto_mes)]
ggplot(data = prestamos, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Promedio de monto de Deudas por grupo de clientes (normalizado)",
        x = "Meses", y = "Promedio de monto de Deuda",
        colour = "Grupos"
    ) +
    theme_classic()


# Cash-flow in
cashflow_in <- dataset[, sum(mtransferencias_emitidas) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
cashflow_in[, foto_mes := as.character(foto_mes)]
ggplot(data = cashflow_in, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Media de flujo entrante de dinero por grupo de clientes (normalizado)",
        x = "Meses", y = "Media de ingreso de dinero",
        colour = "Grupos"
    ) +
    theme_classic()

cashflow_out <- dataset[mtransferencias_recibidas < 200000, sum(mtransferencias_recibidas) / length(numero_de_cliente), by = c("cluster2", "foto_mes")]
cashflow_out[, foto_mes := as.character(foto_mes)]
ggplot(data = cashflow_out, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(
        title = "Media de flujo saliente de dinero por grupo de clientes (normalizado)",
        x = "Meses", y = "Media de egreso",
        colour = "Grupos"
    ) +
    theme_classic()


dataset[, mean(ctrx_quarter), cluster2]
dataset[, mean(mpayroll), cluster2]
dataset[, mean(internet), cluster2]
dataset[, median(cproductos), cluster2]
dataset[, mean(mrentabilidad), cluster2]
dataset[, mean(mcomisiones), cluster2]
dataset[, mean(cinversion1), cluster2]
dataset[, mean(cinversion2), cluster2]

summary(dataset[, ccheques_depositados_rechazados, cluster2])
summary(dataset[, cproductos, cluster2])
dataset[cprestamos_personales > 0, mean(cprestamos_personales), cluster2]
dataset[cprestamos_prendarios > 0, mean(cprestamos_prendarios), cluster2]
dataset[cprestamos_hipotecarios > 0, mean(cprestamos_hipotecarios), cluster2]
dataset[cproductos > 0, mean(cproductos), cluster2]
summary(dataset[(cproductos > 0) & (cluster2 == 1)])



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


distribucion_clientes_cluster <- dataset[, length(numero_de_cliente), by = c("cluster2", "foto_mes")]
distribucion_clientes_cluster[, foto_mes := as.character(foto_mes)]
ggplot(data = distribucion_clientes_cluster, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()


rentabilidades <- dataset[, sum(mrentabilidad)/length(mrentabilidad), by = c("cluster2", "foto_mes")]
rentabilidades[, foto_mes := as.character(foto_mes)]
ggplot(data = rentabilidades, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()


comisiones <- dataset[, sum(mcomisiones)/length(mcomisiones), by = c("cluster2", "foto_mes")]
comisiones[, foto_mes := as.character(foto_mes)]
ggplot(data = comisiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()


inversiones <- dataset[, sum(cinversion2), by = c("cluster2", "foto_mes")]
inversiones[, foto_mes := as.character(foto_mes)]
ggplot(data = inversiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()





inversiones <- dataset[, sum(mrentabilidad_annual) / length(mrentabilidad_annual), by = c("cluster2", "foto_mes")]
inversiones[, foto_mes := as.character(foto_mes)]
ggplot(data = inversiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()


inversiones <- dataset[, sum(chomebanking_transacciones) / length(mcuentas_saldo), by = c("cluster2", "foto_mes")]
inversiones[, foto_mes := as.character(foto_mes)]
ggplot(data = inversiones, aes(x = foto_mes, y = V1, group = cluster2)) +
    geom_line(aes(colour = factor(cluster2))) +
    geom_point() +
    labs(colour = "Grupos") +
    scale_fill_discrete(labels = c("A", "B", "C", "D")) +
    theme_classic()


#saldos <- dataset[, sum(mcuentas_saldo), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(mcaja_ahorro), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(ctrx_quarter), by = c("cluster2", "foto_mes")]
#saldos <- dataset[, sum(cpayroll_trx), by = c("cluster2", "foto_mes")]
saldos <- dataset[, sum(cpayroll_trx), by = c("cluster2", "foto_mes")]
saldos[, foto_mes := as.character(foto_mes)]



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
