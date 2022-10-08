#require vm con
#   8 vCPU
#  64 GB  memoria RAM
# 256 GB  espacio en disco


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("scales")
require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "DR9130"
PARAM$dataset  <- "./exp/CA9060/dataset.csv.gz"
# FIN Parametros del script

#------------------------------------------------------------------------------

graficar_campo  <- function( campo, periodos_analisis )
{

  #quito de grafico las colas del 5% de las densidades

  tb_quantiles  <- data.table( qmin= numeric(),  qmax= numeric() )

  for( periodo in periodos_analisis )
  {
    qu  <- quantile(  dataset[ foto_mes==periodo , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
    tb_quantiles  <- rbind( tb_quantiles, use.names=FALSE, as.list(qu) )
  }

  xxmin  <- tb_quantiles[ , min( qmin ) ]
  xxmax  <- tb_quantiles[ , max( qmax ) ]

  yymax  <- 0
  for( per in periodos_analisis )
  {
    den  <- density( dataset[ foto_mes==per, get(campo) ],
                     kernel="gaussian", na.rm=TRUE )

    mayor  <- max( den$y )
    if( mayor > yymax ) yymax <- mayor 
  }

  densidad_A  <- density( dataset[ foto_mes==periodos_analisis[1], get(campo) ],
                          kernel="gaussian", na.rm=TRUE )


  plot( densidad_A,
        col= GLOBAL_colores[1],
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, yymax ),
        main= paste0( campo  ) 
      )

  for( per in 2:length(periodos_analisis) )
  {
    densidad_B  <- density( dataset[ foto_mes==periodos_analisis[ per ], get(campo) ],
                            kernel="gaussian", na.rm=TRUE )

    lines(densidad_B, col= GLOBAL_colores[per], lty=1)
  }
  
  legend(  "topright",  
           legend= periodos_analisis,
           col=GLOBAL_colores, lty=1 )

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("~/buckets/b1/")

#cargo el dataset donde voy a entrenar
dataset  <- fread( PARAM$dataset )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes<=202103, 
         clase01 :=  ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]


campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )
#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ foto_mes %in% c( 202103, 202104, 202105), campos_buenos, with=FALSE]),
                        label= dataset[foto_mes %in% c( 202103, 202104, 202105), clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=             31,
                                   learning_rate=        0.018,
                                   num_iterations=     526,
                                   num_leaves=        1608,
                                   min_data_in_leaf=  5200,
                                   feature_fraction=     0.6,
                                   seed=              102191
                                  )
                    )

tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
campos_ordenados  <- c( tb_importancia$Feature,  setdiff( colnames(dataset), tb_importancia$Feature ) )
campos_ordenados  <-  setdiff(  campos_ordenados,  c( "foto_mes","clase_ternaria","clase01" ) )



dataset[  , foto_mes := as.character( foto_mes ) ]


periodos_analisis  <- c( 202103, 202104, 202105, 202107 )
GLOBAL_colores <-  viridis_pal()(length( periodos_analisis ) )

pdf("densidades_orignales.pdf")

for( campo in  campos_ordenados )
{
  cat( campo, "  " )

  graficar_campo( campo, periodos_analisis )
}

dev.off()

