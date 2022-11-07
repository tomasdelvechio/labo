#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262_v1"
PARAM$exp_input <- "FE9250_exp2" # Uso mi mejor Dataset de la C3
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

# cargo el dataset donde voy a entrenar
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset.csv.gz")
dataset <- fread(dataset_input)

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset[, clase_ternaria := NULL] )
dataset <- cbind(dataset, dataset12$clase_ternaria)

# Los campos a tener en cuenta seran los 15 con mejor Feature Importance de mi
#   mejor experimento privado en la competencia 3
campos_buenos <- c("ctrx_quarter_normalizado", "ctrx_quarter", "ctrx_quarter_lag1",
  "mcuentas_saldo_rank", "ctrx_quarter_normalizado_lag2", "mcaja_ahorro_rank",
  "mpayroll_sobre_edad_rank", "ctrx_quarter_normalizado_lag1", "cpayroll_trx",
  "mprestamos_personales_rank", "mtarjeta_visa_consumo_rank", "mpayroll_rank",
  "mpasivos_margen_rank", "ctrx_quarter_lag2", "ctrx_quarter_normalizado_avg6",
  "cliente_vip", "internet", "cliente_edad", "cliente_antiguedad", "mrentabilidad",
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
  "minversion1_dolares", "cinversion2", "minversion2")
  
#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )


#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

# Quiero entre 3 y 4 clusters
while(  h>0  &  !( distintos >=3 & distintos <=4 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

#dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
#dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
#dataset[  , mean(mcuentas_saldo),  cluster2 ]
#dataset[  , mean(chomebanking_transacciones),  cluster2 ]


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )
