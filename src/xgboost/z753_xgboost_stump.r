# XGBoost  sabor tradicional
# ensemble de 400 arboles de APENAS altura 1  (decision stumps), la raiz y dos hojas

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ foto_mes==202103 , campos_buenos, with=FALSE]),
                        label= dataset[ foto_mes==202103, clase01] )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( objective=       "binary:logistic",
                                   max_depth=           1,    #arboles de altura 1, solo dos hojas !
                                   min_child_weight=   10 ),
                      nrounds= 400
                    )


#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dataset[ foto_mes==202105, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dataset[ foto_mes==202105 , numero_de_cliente],
                                 "Predicted"= as.integer( prediccion > 1/40) )  ) #genero la salida

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA7530/", showWarnings = FALSE )
archivo_salida  <- "./exp/KA7530/KA7530_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
