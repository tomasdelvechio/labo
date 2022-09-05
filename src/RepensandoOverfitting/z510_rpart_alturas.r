#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("." )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

dataset[ foto_mes == 202101 ,
				clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]

dataset_entrenar <- dataset[ foto_mes == 202101 ]
dataset_aplicar <- dataset[ foto_mes == 202103 ]

for( vmaxdepth  in 4:25 )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ . -clase_ternaria -mcomisiones_mantenimiento -Visa_mpagado",
                   data= dataset_entrenar,
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= 0,
                   minsplit= 5,
                   maxdepth=  vmaxdepth
                  )

  #aplico el modelo a los datos en donde entrene
  prediccion_202101  <- predict( object=  modelo,
																 newdata= dataset_entrenar,
																 type = "prob")
  ganancia_202101 <-  sum(  (prediccion_202101[, "POS"] > 0.025) *
                            ifelse( dataset_entrenar$clase_binaria=="POS", 78000, -2000 ) )

  cat( vmaxdepth, "\t", ganancia_202101, "\n" )

  prediccion_202103  <- predict( modelo, dataset_aplicar, type = "prob")

  prob_pos  <- prediccion_202103[, "POS"]
  estimulo  <- as.numeric(prob_pos > 0.025)

  entrega <-  as.data.table( list(  "numero_de_cliente"= dataset_aplicar$numero_de_cliente,
                                    "Predicted"=  estimulo ) )

  #genero el archivo para Kaggle
  fwrite( entrega,
          file= paste0("./kaggle/altura_", vmaxdepth, ".csv"))
}

