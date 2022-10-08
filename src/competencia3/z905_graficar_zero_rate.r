#Necesita para correr en Google Cloud
#  32 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

#Este script genera graficos que muestra que para algunos meses, ciertas variables
#  fueron pisadas con CEROS por el sector deIT que genera el DataWarehouse

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "CA9050"
PARAM$dataset  <- "./datasets/competencia3_2022.csv.gz"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

#copio si hace falta el dataset

setwd("~/buckets/b1/")

#cargo el dataset
dataset  <- fread( PARAM$dataset )   #donde entreno


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#ordeno el dataset
setorder( dataset,  foto_mes, numero_de_cliente )

campos_buenos  <- setdiff( colnames( dataset), 
                           c("numero_de_cliente","foto_mes","clase_ternaria" ) )


#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el ratio de ceros que tiene esa variable

pdf("zeroes_ratio.pdf")

for( campo in  campos_buenos )
{
  tbl <- dataset[ foto_mes<=202107,
                  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) ,
                  foto_mes ]

  ymin <-  min( tbl$zero_ratio )
  ymax <-  max( tbl$zero_ratio )
  if( ymin == 0 )  ymin <- -0.1
  if( ymax == 0 )  ymax <-  0.1

  plot(x= 1:nrow(tbl),
       y= tbl$zero_ratio,
       type= "o",
       main= paste0("Zeroes ratio  -  ",  campo),
       xlab= "Periodo",
       ylab= "Zeroes  ratio",
       ylim= c( ymin, ymax ),
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25), col=c("green","green","green"), lty=c(1,1,1), lwd=c(1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
}

dev.off()

#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el ratio de NAs que tiene esa variable

pdf("nas_ratio.pdf")

for( campo in  campos_buenos )
{
  tbl <- dataset[ foto_mes<=202107,
                  list( "na_ratio" = sum( is.na(get(campo)), na.rm=TRUE)/.N ) , 
                  foto_mes ]

  ymin <-  min( tbl$na_ratio )
  ymax <-  max( tbl$na_ratio )
  if( ymin == 0 )  ymin <- -0.1
  if( ymax == 0 )  ymax <-  0.1

  plot(x= 1:nrow(tbl),
       y= tbl$na_ratio,
       type= "o",
       main= paste0("NAs ratio  -  ",  campo),
       xlab= "Periodo",
       ylab= "NAs  ratio",
       ylim= c( ymin, ymax ),
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25), col=c("green","green","green"), lty=c(1,1,1), lwd=c(1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
}

dev.off()

#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el promedio de esa variable

pdf("promedios.pdf")

for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202107 ,
                    list( "promedio" = mean( get(campo), na.rm=TRUE)) ,
                    foto_mes ]

  ceros <- dataset[ foto_mes<=202107 ,
                    list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) ,
                    foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$promedio,
       type= "o",
       main= paste0("Promedios  -  ",  campo),
       xlab= "Periodo",
       ylab= "Promedio",
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25), col=c("green","green","green"), lty=c(1,1,1), lwd=c(1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
  
  for( i in 1:nrow(tbl) )
  {
    if( ceros[ i, zero_ratio]> 0.99 &  median(ceros[ , zero_ratio]) < 0.99 )
    {
      abline(v=c(i), col=c("red"), lty=c(1), lwd=c(1))
    }
  }
}

dev.off()

#------------------------------------------------------------------------------
#Para cada variable , grafico para cada mes el promedio de esa variable cuando la variable es DISTINTA de cero

pdf("promedios_nocero.pdf")

for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202107 & get(campo)!=0,
           list( "promedio" = mean( get(campo), na.rm=TRUE)) ,
           foto_mes ]

  ceros <- dataset[ foto_mes<=202107 ,
                    list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) ,
                    foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$promedio,
       type= "o",
       main= paste0("Promedios NO cero -  ",  campo),
       xlab= "Periodo",
       ylab= "Promedio valores no cero",
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25), col=c("green","green","green"), lty=c(1,1,1), lwd=c(1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
  
  for( i in 1:nrow(tbl) )
  {
    if( ceros[ i, zero_ratio]> 0.99 &  median(ceros[ , zero_ratio]) < 0.99 )
    {
      abline(v=c(i), col=c("red"), lty=c(1), lwd=c(1))
    }
  }
}

dev.off()

#------------------------------------------------------------------------------

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE  )

