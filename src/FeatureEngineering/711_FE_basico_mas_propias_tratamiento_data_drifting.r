#Necesita para correr en Google Cloud
# 32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm(list = ls())  #remove all objects
gc()             #garbage collection

require("data.table")


options(error = function() { 
  traceback(20)
  options(error = NULL)
  stop("exiting after script error") 
})



#setwd( "~/buckets/b1/" )
setwd("/home/tomas/workspace/uba/dmeyf")

#cargo el dataset
dataset <- fread("./datasets/competencia2_original_2022.csv.gz")

#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/FE7110/", showWarnings = FALSE)
setwd("./exp/FE7110/")   #Establezco el Working Directory DEL EXPERIMENTO

#INICIO de la seccion donde se deben hacer cambios con variables nuevas

#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset[, ctrx_quarter_normalizado := ctrx_quarter]
dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter_normalizado)]
dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
dataset[cliente_antiguedad == 3, ctrx_quarter_normalizado := ctrx_quarter * 1.2]

#variable extraida de una tesis de maestria de Irlanda
dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[, mv_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
dataset[, mv_status02 := Master_status + Visa_status]
dataset[, mv_status03 := pmax(ifelse(is.na(Master_status), 10, Master_status), ifelse(is.na(Visa_status), 10, Visa_status))]
dataset[, mv_status04 := ifelse(is.na(Master_status), 10, Master_status) + ifelse(is.na(Visa_status), 10, Visa_status)]
dataset[, mv_status05 := ifelse(is.na(Master_status), 10, Master_status) + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

dataset[, mv_status06 := ifelse(is.na(Visa_status),
                                ifelse(is.na(Master_status),
                                    10,
                                    Master_status), 
                                Visa_status)]

dataset[, mv_status07 := ifelse(is.na(Master_status), 
                                ifelse(is.na(Visa_status), 10, Visa_status), 
                                Master_status)]


#combino MasterCard y Visa
dataset[, mv_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

dataset[, mv_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
dataset[, mv_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
dataset[, mv_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
dataset[, mv_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
dataset[, mv_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
dataset[, mv_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
dataset[, mv_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
dataset[, mv_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
dataset[, mv_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
dataset[, mv_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
dataset[, mv_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
dataset[, mv_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
dataset[, mv_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
dataset[, mv_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
dataset[, mv_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
dataset[, mv_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
dataset[, mv_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
dataset[, mv_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
dataset[, mv_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[, mvr_Master_mlimitecompra := Master_mlimitecompra / mv_mlimitecompra]
dataset[, mvr_Visa_mlimitecompra   := Visa_mlimitecompra / mv_mlimitecompra]
dataset[, mvr_msaldototal          := mv_msaldototal / mv_mlimitecompra]
dataset[, mvr_msaldopesos          := mv_msaldopesos / mv_mlimitecompra]
dataset[, mvr_msaldopesos2         := mv_msaldopesos / mv_msaldototal]
dataset[, mvr_msaldodolares        := mv_msaldodolares / mv_mlimitecompra]
dataset[, mvr_msaldodolares2       := mv_msaldodolares / mv_msaldototal]
dataset[, mvr_mconsumospesos       := mv_mconsumospesos / mv_mlimitecompra]
dataset[, mvr_mconsumosdolares     := mv_mconsumosdolares / mv_mlimitecompra]
dataset[, mvr_madelantopesos       := mv_madelantopesos / mv_mlimitecompra]
dataset[, mvr_madelantodolares     := mv_madelantodolares / mv_mlimitecompra]
dataset[, mvr_mpagado              := mv_mpagado / mv_mlimitecompra]
dataset[, mvr_mpagospesos          := mv_mpagospesos / mv_mlimitecompra]
dataset[, mvr_mpagosdolares        := mv_mpagosdolares / mv_mlimitecompra]
dataset[, mvr_mconsumototal        := mv_mconsumototal  / mv_mlimitecompra]
dataset[, mvr_mpagominimo          := mv_mpagominimo  / mv_mlimitecompra]

#Aqui debe usted agregar sus propias nuevas variables
dataset[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
dataset[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]

dataset[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
dataset[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]

dataset[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
dataset[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]

dataset[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
dataset[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

## Arma secreta con arbol excluyendo a ctrx_quarter
dataset[, cd_v2_1 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales < 14.851E+3)]
dataset[, cd_v2_2 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro < 133.32 & mprestamos_personales >= 14.851E+3)]

dataset[, cd_v2_3 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo < 3572.7)]
dataset[, cd_v2_4 := as.integer(mpasivos_margen < 64.74 & mcaja_ahorro >= 133.32 & mtarjeta_visa_consumo >= 3572.7)]

dataset[, cd_v2_5 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll < 4693.1)]
dataset[, cd_v2_6 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa < 1 & mpayroll >= 4693.1)]

dataset[, cd_v2_7 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen < 296.68)]
dataset[, cd_v2_8 := as.integer(mpasivos_margen >= 64.74 & ctarjeta_visa >= 1 & mpasivos_margen >= 296.68)]

## Features de prueba

dataset[, campo_prueba1 := ctrx_quarter * mcuentas_saldo]
dataset[, campo_prueba2 := ctrx_quarter * mcuenta_corriente]
dataset[, campo_prueba3 := ctrx_quarter * mprestamos_personales]
dataset[, campo_prueba4 := ctrx_quarter * ccomisiones_otras]
dataset[, campo_prueba5 := ctrx_quarter * active_quarter]
dataset[, campo_prueba6 := mcuentas_saldo * ctrx_quarter]
dataset[, campo_prueba7 := mcuentas_saldo * mcuenta_corriente]
dataset[, campo_prueba8 := mcuentas_saldo * mprestamos_personales]
dataset[, campo_prueba9 := mcuentas_saldo * ccomisiones_otras]
dataset[, campo_prueba10 := mcuentas_saldo * active_quarter]
dataset[, campo_prueba11 := mcuenta_corriente * ctrx_quarter]
dataset[, campo_prueba12 := mcuenta_corriente * mcuentas_saldo]
dataset[, campo_prueba13 := mcuenta_corriente * mprestamos_personales]
dataset[, campo_prueba14 := mcuenta_corriente * ccomisiones_otras]
dataset[, campo_prueba15 := mcuenta_corriente * active_quarter]
dataset[, campo_prueba16 := mprestamos_personales * ctrx_quarter]
dataset[, campo_prueba17 := mprestamos_personales * mcuentas_saldo]
dataset[, campo_prueba18 := mprestamos_personales * mcuenta_corriente]
dataset[, campo_prueba19 := mprestamos_personales * ccomisiones_otras]
dataset[, campo_prueba20 := mprestamos_personales * active_quarter]
dataset[, campo_prueba21 := ccomisiones_otras * ctrx_quarter]
dataset[, campo_prueba22 := ccomisiones_otras * mcuentas_saldo]
dataset[, campo_prueba23 := ccomisiones_otras * mcuenta_corriente]
dataset[, campo_prueba24 := ccomisiones_otras * mprestamos_personales]
dataset[, campo_prueba25 := ccomisiones_otras * active_quarter]
dataset[, campo_prueba26 := active_quarter * ctrx_quarter]
dataset[, campo_prueba27 := active_quarter * mcuentas_saldo]
dataset[, campo_prueba28 := active_quarter * mcuenta_corriente]
dataset[, campo_prueba29 := active_quarter * mprestamos_personales]
dataset[, campo_prueba30 := active_quarter * ccomisiones_otras]

#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos <- lapply(names(dataset), function(.name) dataset[, sum(is.infinite(get(.name)))])
infinitos_qty <- sum(unlist(infinitos))
if (infinitos_qty > 0) {
  cat("ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n")
  dataset[mapply(is.infinite, dataset)] <- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans <- lapply(names(dataset), function(.name) dataset[, sum(is.nan(get(.name)))])
nans_qty <- sum(unlist(nans))
if (nans_qty > 0) {
  cat("ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n")
  cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
}

#--------------------------------------
# Se eliminan las variables que se considera, a ojo, que tienen data drifting

vars_tratar_data_drifting <- list(
    "mcuentas_saldo",
    "mcuenta_corriente",
    "mv_mpagominimo",
    "mv_Fvencimiento",
    "ccajas_otras",
    "mvr_mpagosdolares",
    "mv_mpagosdolares",
    "mv_mpagado",
    "Master_mpagado",
    "mvr_mpagado",
    "mvr_msaldodolares",
    "Visa_mpagosdolares",
    "mv_msaldodolares",
    "Visa_msaldodolares",
    "mvr_mconsumosdolares",
    "mcuenta_debitos_automaticos",
    "mforex_sell",
    "Master_mfinanciacion_limite",
    "Master_Finiciomora",
    "Master_fultimo_cierre",
    "Master_mpagominimo",
    "Visa_Finiciomora",
    "Visa_madelantodolares",
    "Visa_fultimo_cierre",
    "mv_Finiciomora",
    "mv_madelantodolares",
    "mv_fultimo_cierre"
)

#for (variable in vars_tratar_data_drifting) {
    #dataset[, (paste("frank__", var)) := (frank(var) - mean(var)) / sqrt(var(var)), by = dataset$foto_mes]
#    dataset[, (paste("frank__", variable, sep="")) := (frank(variable) - 1) / (.N - 1), by = dataset$foto_mes]
    #dataset[, (paste("frank__", var)) := (frank(var) - 1) / (.N - 1)]
#    dataset[, (variable) := NULL]
#}

#--------------------------------------
#grabo el dataset
fwrite(dataset,
        "dataset_7110.csv.gz",
        logical01 = TRUE,
        sep = ",")
