#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202101 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202101 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202001", "202003"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

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


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6130/", showWarnings = FALSE )
setwd("./exp/DR6130/")



pdf("densidades_01_03.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

