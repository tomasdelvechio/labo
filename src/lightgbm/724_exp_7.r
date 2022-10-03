# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA7240"

PARAM$input$dataset <- "./exp/FE7110/dataset_7110.csv.gz"
PARAM$input$training <- c(202103)
PARAM$input$future <- c(202105)

PARAM$finalmodel$max_bin <- 31
PARAM$finalmodel$learning_rate <- 0.023463737229234
PARAM$finalmodel$num_iterations <- 209
PARAM$finalmodel$num_leaves <- 148 # 784
PARAM$finalmodel$min_data_in_leaf <- 79
PARAM$finalmodel$feature_fraction <- 0.350337184048083
PARAM$finalmodel$lambda_l1 <- 1.71277329721618
PARAM$finalmodel$lambda_l2 <- 72.7279631157529
PARAM$finalmodel$min_gain_to_split <- 11.0664622222673
PARAM$finalmodel$semilla <- 906007

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("/home/tomas/workspace/uba/dmeyf")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
    data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
    label = dataset[train == 1L, clase01]
)

# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo <- lgb.train(
    data = dtrain,
    param = list(
        objective = "binary",
        max_bin = PARAM$finalmodel$max_bin,
        learning_rate = PARAM$finalmodel$learning_rate,
        num_iterations = PARAM$finalmodel$num_iterations,
        num_leaves = PARAM$finalmodel$num_leaves,
        min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
        feature_fraction = PARAM$finalmodel$feature_fraction,
        seed = PARAM$finalmodel$semilla
    )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
    file = archivo_importancia,
    sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
    file = "prediccion.txt",
    sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq(6000, 12000, by = 50)
for (envios in cortes)
{
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]

    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
        file = paste0(PARAM$experimento, "_", envios, ".csv"),
        sep = ","
    )
}

#--------------------------------------

#quit(save = "no")
