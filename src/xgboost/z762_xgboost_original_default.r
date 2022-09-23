# Este script esta pensado para correr en la PC local 
# 5-fold cross validation

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("xgboost")


ksemilla_azar  <- 102191  #Aqui poner la propia semilla

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

SCORE_PCORTE  <- log( 1/40 / ( 1 - 1/40 ) )   #esto hace falta en ESTA version del XGBoost ... misterio por ahora ...

fganancia_logistic_xgboost   <- function( scores, datos) 
{
  vlabels  <- getinfo( datos, "label")

  gan  <- sum( ( scores > SCORE_PCORTE  ) *
                 ifelse( vlabels== 1, 78000, -2000 ) )


  return(  list("metric" = "ganancia", "value" = gan ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_xgboost  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  SCORE_PCORTE  <<- log( x$prob_corte / ( 1 - x$prob_corte ) ) 

  kfolds  <- 5   # cantidad de folds para cross validation

  #otros hiperparmetros, que por ahora dejo en su valor default
  param_basicos  <- list( gamma=                0.0,  #por ahora, lo dejo fijo, equivalente a  min_gain_to_split
                          alpha=                0.0,  #por ahora, lo dejo fijo, equivalente a  lambda_l1
                          lambda=               0.0,  #por ahora, lo dejo fijo, equivalente a  lambda_l2
                          subsample=            1.0,  #por ahora, lo dejo fijo
                          tree_method=       "auto",  #por ahora lo dejo fijo, pero ya lo voy a cambiar a "hist"
                          grow_policy=  "depthwise",  #ya lo voy a cambiar a "lossguide"
                          max_bin=            256,    #por ahora fijo
                          max_leaves=           0,    #ya lo voy a cambiar
                          scale_pos_weight=     1.0   #por ahora, lo dejo fijo
                        )

  param_completo  <- c( param_basicos, x )

  set.seed( 999983 )
  modelocv  <- xgb.cv( objective= "binary:logistic",
                       data= dtrain,
                       feval= fganancia_logistic_xgboost,
                       disable_default_eval_metric= TRUE,
                       maximize= TRUE,
                       stratified= TRUE,     #sobre el cross validation
                       nfold= kfolds,        #folds del cross validation
                       nrounds= 9999,        #un numero muy grande, lo limita early_stopping_rounds
                       early_stopping_rounds= as.integer(50 + 5/x$eta),
                       base_score= mean( getinfo(dtrain, "label")),  
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia   <- unlist( modelocv$evaluation_log[ , test_ganancia_mean] )[ modelocv$best_iter ] 

  ganancia_normalizada  <- ganancia* kfolds     #normailizo la ganancia

  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("nrounds"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$nrounds <- modelocv$best_iter  #asigno el mejor nrounds
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT7620/", showWarnings = FALSE )
setwd("./exp/HT7620/")   #Establezco el Working Directory DEL EXPERIMENTO


#en estos archivos quedan los resultados
klog        <- "HT7620.txt"


GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes==202103, clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data=  data.matrix( dataset[ foto_mes==202103, campos_buenos, with=FALSE ] ),
                        label= dataset[ foto_mes==202103, clase01 ] )


#llamo con los parametros por default
x  <- list( eta=               0.3,
            colsample_bytree=  1.0,
            min_child_weight=  1.0,
            max_depth=         6,
            prob_corte=        1/60
          )

EstimarGanancia_xgboost( x ) 
