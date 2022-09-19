#Analisis de  Concept Drifting y Data Drifting
# utilizando el dataset de la competencia 2 que tiene completos 202101 y 202103

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/")  #Establezco el Working Directory

#cargo el dataset de la competencia 2  , que tiene lo que paso en 202103
dataset  <- fread("./datasets/competencia2_2022.csv.gz")  #donde entreno
dataset  <- dataset[  foto_mes %in% c( 202101, 202103 ) ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202101 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


cantidad_hojas  <- sum( modelo$frame$var == "<leaf>" )

#aplico el modelo a TODOS los datos, incluso donde entreno
prediccion  <- predict( object=  modelo,
                        newdata= dataset,
                        type = "prob")

#le pego la prediccion al dataset
dataset[  , prob_SI := prediccion[ , "SI"]  ]


dataset[  , gan := ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]

tb_hojas  <-  dataset[  , list( reg_202101 = sum( ifelse( foto_mes==202101, 1, 0 ) ),
                                reg_202103 = sum( ifelse( foto_mes==202103, 1, 0 ) ),
                                b2_202101  = sum( ifelse( foto_mes==202101 & clase_ternaria=="BAJA+2", 1, 0 ) ),
                                b2_202103  = sum( ifelse( foto_mes==202103 & clase_ternaria=="BAJA+2", 1, 0 ) ),
                                gan_202101 = sum( ifelse( foto_mes==202101, gan, 0 ) ),
                                gan_202103 = sum( ifelse( foto_mes==202103, gan, 0 ) ) ),
                         by= prob_SI ]


#calculo las ganancias en 202101 y  202103, que es donde cae
tb_hojas[  gan_202101>0,
           list( sum( gan_202101 ),  sum( gan_202103 ) ) ]

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6120/", showWarnings = FALSE )
setwd("./exp/DR6120/")

setorder( tb_hojas,  -prob_SI )

tb_hojas[  , regacum_202101 := cumsum( reg_202101 ) ]
tb_hojas[  , regacum_202103 := cumsum( reg_202103 ) ]
tb_hojas[  , ganacum_202101 := cumsum( gan_202101 ) ]
tb_hojas[  , ganacum_202103 := cumsum( gan_202103 ) ]


fwrite( tb_hojas,
        file= "tb_hojas.txt",
        sep= "\t" )
        
