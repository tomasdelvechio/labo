#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/tomas/workspace/uba/dmeyf")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
# opt bayesiana v1
#modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                 data=   dtrain,  #los datos donde voy a entrenar
#                 xval=        0,
#                 cp=         -1,   #esto significa no limitar la complejidad de los splits
#                 minsplit=  128,     #minima cantidad de registros para que se haga el split
#                 minbucket=  19,     #tamaño minimo de una hoja
#                 maxdepth=    5 )    #profundidad maxima del arbol

#genero el modelo,  aqui se construye el arbol
# opt bayesiana v2
#modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                 data=   dtrain,  #los datos donde voy a entrenar
#                 xval=        0,
#                 cp=         -1,   #esto significa no limitar la complejidad de los splits
#                 minsplit=  412,     #minima cantidad de registros para que se haga el split
#                 minbucket= 269,     #tamaño minimo de una hoja
#                 maxdepth=    5 )    #profundidad maxima del arbol

#genero el modelo,  aqui se construye el arbol
# opt bayesiana v3
#modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                 data=   dtrain,  #los datos donde voy a entrenar
#                 xval=        0,
#                 cp=      0.217,   #esto significa no limitar la complejidad de los splits
#                 minsplit= 1181,     #minima cantidad de registros para que se haga el split
#                 minbucket= 492,     #tamaño minimo de una hoja
#                 maxdepth=    5 )    #profundidad maxima del arbol

#genero el modelo,  aqui se construye el arbol
# opt bayesiana v3.1
#modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                 data=   dtrain,  #los datos donde voy a entrenar
#                 xval=        0,
#                 cp=         -1,   #esto significa no limitar la complejidad de los splits
#                 minsplit= 1181,     #minima cantidad de registros para que se haga el split
#                 minbucket= 492,     #tamaño minimo de una hoja
#                 maxdepth=    5 )    #profundidad maxima del arbol

#genero el modelo,  aqui se construye el arbol
# opt bayesiana v3.2
#modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                 data=   dtrain,  #los datos donde voy a entrenar
#                 xval=        0,
#                 cp=         -1,   #esto significa no limitar la complejidad de los splits
#                 minsplit= 1221,     #minima cantidad de registros para que se haga el split
#                 minbucket= 274,     #tamaño minimo de una hoja
#                 maxdepth=    5 )    #profundidad maxima del arbol

#genero el modelo,  aqui se construye el arbol
# opt bayesiana id 1 C2 prueba_1
# ver: https://docs.google.com/spreadsheets/d/1A-gKNeRQGcd-hT3h98S3cFO6mXFIWG8Ix47mqfVugWA/edit#gid=0
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=   dtrain,  #los datos donde voy a entrenar
                 xval=        5,
                 cp=      -0.61,   #esto significa no limitar la complejidad de los splits
                 minsplit= 1171,     #minima cantidad de registros para que se haga el split
                 minbucket= 326,     #tamaño minimo de una hoja
                 maxdepth=   20 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
#dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , prob_baja2 := prediccion[, "SI"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/_K101_004_opt_bay_vC1_binaria.csv",
        #file= "./exp/KA2001/borrar.csv",
        sep=  "," )

