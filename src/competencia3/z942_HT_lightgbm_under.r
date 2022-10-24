#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# pensado para datasets con UNDERSAPLING de la clase mayoritaria

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#Parametros del script
PARAM  <- list()
PARAM$experimento <- "HT9420"

PARAM$exp_input  <- "TS9320"
# FIN Parametros del script


#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------

ksemilla  <- 102191

kcrossvalidation_folds  <- 5  #En caso que se haga cross validation, se usa esta cantidad de folds

#Hiperparametros FIJOS de  lightgbm
param_lgb_basicos  <- list( 
   boosting= "gbdt",               #puede ir  dart  , ni pruebe random_forest
   objective= "binary",
   metric= "custom",
   first_metric_only= TRUE,
   boost_from_average= TRUE,
   feature_pre_filter= FALSE,
   force_row_wise= TRUE,           #para que los alumnos no se atemoricen con tantos warning
   verbosity= -100,
   max_depth=  -1,                 # -1 significa no limitar,  por ahora lo dejo fijo
   min_gain_to_split= 0.0,         #por ahora, lo dejo fijo
   min_sum_hessian_in_leaf= 0.001, #por ahora, lo dejo fijo
   lambda_l1= 0.0,                 #por ahora, lo dejo fijo
   lambda_l2= 0.0,                 #por ahora, lo dejo fijo
   max_bin= 31,                    #por ahora, lo dejo fijo
   num_iterations= 9999,           #un numero muy grande, lo limita early_stopping_rounds

   bagging_fraction= 1.0,          #por ahora, lo dejo fijo
   pos_bagging_fraction= 1.0,      #por ahora, lo dejo fijo
   neg_bagging_fraction= 1.0,      #por ahora, lo dejo fijo

   drop_rate=  0.1,                #solo se activa en  dart
   max_drop= 50,                   #solo se activa en  dart
   skip_drop= 0.5,                 #solo se activa en  dart

   extra_trees= FALSE,

   seed=  ksemilla
   )


#Aqui se cargan los hiperparametros que se optimizan en la Bayesian Optimization
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.01, upper=  0.3),
         makeNumericParam("feature_fraction", lower=    0.2 , upper=  0.8),
         makeNumericParam("coverage",         lower=    0.05, upper=  1.0),
         makeNumericParam("leaf_size_log",    lower=    1.0 , upper= 12.0)
        )


#si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
kBO_iteraciones  <- 50  #iteraciones de la Optimizacion Bayesiana

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

exp_log  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
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

vprob_optima  <- c()

fganancia_lgbm_meseta  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")

  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 , 78000, -2000 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]

  gan  <-  tbl[ , max(gan_acum) ]

  pos  <- which.max(  tbl[ , gan_acum ] ) 
  vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )

  rm( tbl )
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm  <- function( x )
{
  gc()
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  param_completo  <- c( param_lgb_basicos,  x )

  param_completo$early_stopping_rounds  <- as.integer(200 + 4/param_completo$learning_rate )
  
  #Primero defino el tamaño de las hojas
  param_completo$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ x$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  param_completo$num_leaves  <-  pmin( 131072, pmax( 2,  round(x$coverage * nrow( dtrain ) / param_completo$min_data_in_leaf ) ) )
  cat( "min_data_in_leaf:", param_completo$min_data_in_leaf,  ",  num_leaves:", param_completo$num_leaves, "\n" )

  vprob_optima  <<- c()
  set.seed( param_completo$seed )
  modelo_train  <- lgb.train( data= dtrain,
                              valids= list( valid= dvalidate ),
                              eval=   fganancia_lgbm_meseta,
                              param=  param_completo,
                              verbose= -100 )

  prob_corte  <- vprob_optima[ modelo_train$best_iter ]

  #aplico el modelo a testing y calculo la ganancia
  prediccion  <- predict( modelo_train, 
                          data.matrix( dataset_test[ , campos_buenos, with=FALSE]) )

  tbl  <- dataset_test[ , list(clase_ternaria) ]
  tbl[ , prob := prediccion ]
  ganancia_test  <- tbl[ prob >= prob_corte, 
                         sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]

  cantidad_test_normalizada  <- tbl[ prob >= prob_corte, .N ]

  rm( tbl )
  gc()

  ganancia_test_normalizada  <- ganancia_test


  #voy grabando las mejores column importance
  if( ganancia_test_normalizada >  GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_test_normalizada
    tb_importancia    <- as.data.table( lgb.importance( modelo_train ) )

    fwrite( tb_importancia,
            file= paste0( "impo_", GLOBAL_iteracion, ".txt" ),
            sep= "\t" )

    rm( tb_importancia )
  }


  #logueo final
  ds  <- list( "cols"= ncol(dtrain),  "rows"= nrow(dtrain) )
  xx  <- c( ds, copy(param_completo) )

  #quito los parametros reales
  xx$min_data_in_leaf <- NULL
  xx$num_leaves <- NULL

  xx$early_stopping_rounds  <- NULL
  xx$num_iterations  <- modelo_train$best_iter
  xx$prob_corte  <- prob_corte
  xx$estimulos  <- cantidad_test_normalizada
  xx$ganancia  <- ganancia_test_normalizada
  xx$iteracion_bayesiana  <- GLOBAL_iteracion

  exp_log( xx,  arch= "BO_log.txt" )

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------
#esta es la funcion mas mistica de toda la asignatura
# sera explicada en  Laboratorio de Implementacion III

vprob_optima  <- c()
vpos_optima   <- c()

fganancia_lgbm_mesetaCV  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 & vpesos > 1,
                                               78000,
                                               -2000 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]

  gan  <-  tbl[ , max(gan_acum) ]

  pos  <- which.max(  tbl[ , gan_acum ] ) 
  vpos_optima   <<- c( vpos_optima, pos )
  vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )

  rm( tbl )
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbmCV  <- function( x )
{
  gc()
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  param_completo  <- c(param_lgb_basicos,  x )

  param_completo$early_stopping_rounds  <- as.integer(200 + 4/param_completo$learning_rate )

  #Primero defino el tamaño de las hojas
  param_completo$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ x$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  param_completo$num_leaves  <-  pmin( 131072, pmax( 2,  round(x$coverage * nrow( dtrain ) / param_completo$min_data_in_leaf ) ) )

  vprob_optima  <<- c()
  vpos_optima   <<- c()

  set.seed( param_completo$seed )
  modelocv  <- lgb.cv( data= dtrain,
                       eval=   fganancia_lgbm_mesetaCV,
                       param=  param_completo,
                       stratified= TRUE,                   #sobre el cross validation
                       nfold= kcrossvalidation_folds,
                       verbose= -100 )

  desde  <- (modelocv$best_iter-1)*kcrossvalidation_folds + 1
  hasta  <- desde + kcrossvalidation_folds -1

  prob_corte            <-  mean( vprob_optima[ desde:hasta ] )
  cantidad_normalizada  <-  mean( vpos_optima[ desde:hasta ] ) * kcrossvalidation_folds

  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  ganancia_normalizada  <- ganancia * kcrossvalidation_folds


  #voy grabando las mejores column importance
  if( ganancia_normalizada >  GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_normalizada

    param_impo <-  copy( param_completo )
    param_impo$early_stopping_rounds  <- 0
    param_impo$num_iterations  <- modelocv$best_iter

    modelo  <- lgb.train( data= dtrain,
                       param=  param_impo,
                       verbose= -100 )

    tb_importancia    <- as.data.table( lgb.importance( modelo ) )

    fwrite( tb_importancia,
            file= paste0( "impo_", GLOBAL_iteracion, ".txt" ),
            sep= "\t" )
    
    rm( tb_importancia )
  }


  #logueo final
  ds  <- list( "cols"= ncol(dtrain),  "rows"= nrow(dtrain) )
  xx  <- c( ds, copy(param_completo) )

  #quito los parametros reales
  xx$min_data_in_leaf <- NULL
  xx$num_leaves <- NULL

  xx$early_stopping_rounds  <- NULL
  xx$num_iterations  <- modelocv$best_iter
  xx$prob_corte  <-  prob_corte
  xx$estimulos   <-  cantidad_normalizada
  xx$ganancia  <- ganancia_normalizada
  xx$iteracion_bayesiana  <- GLOBAL_iteracion

  exp_log( xx,  arch= "BO_log.txt" )

  return( ganancia_normalizada )
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd("~/buckets/b1/")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset_training.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset_training.csv.gz" )
dataset  <- fread( dataset_input )

#Verificaciones
if( ! ("fold_train"    %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_train \n")
if( ! ("fold_validate" %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_validate \n")
if( ! ("fold_test"     %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_test  \n")
if( dataset[ fold_train==1, .N ] == 0 ) stop("Error, en el dataset no hay fold_train==1 \n")

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


cat( PARAM$exp_input,
     file= "TrainingStrategy.txt",
     append= FALSE )

#defino la clase binaria clase01
dataset[  , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L ) ]


#los campos que se pueden utilizar para la prediccion
campos_buenos  <- setdiff( copy(colnames( dataset )), c( "clase01", "clase_ternaria", "fold_train", "fold_validate", "fold_test" ) )

#la particion de train siempre va
dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ fold_train==1, campos_buenos, with=FALSE] ),
                        label=   dataset[ fold_train==1, clase01 ],
                        weight=  dataset[ fold_train==1, ifelse( clase_ternaria == "BAJA+2", 1.0000001, 1.0) ],
                        free_raw_data= FALSE
                      )


kvalidate  <- FALSE
ktest  <- FALSE
kcrossvalidation  <- TRUE

#Si hay que hacer validacion
if( dataset[ fold_train==0 & fold_test==0 & fold_validate==1, .N ] > 0 )
{
  kcrossvalidation  <- FALSE
  kvalidate  <- TRUE
  dvalidate  <- lgb.Dataset( data=  data.matrix( dataset[ fold_validate==1, campos_buenos, with=FALSE] ),
                             label= dataset[ fold_validate==1, clase01 ],
                             free_raw_data= FALSE  )

}

#Si hay que hacer testing
if( dataset[ fold_train==0 & fold_validate==0 & fold_test==1, .N ] > 0 )
{
  ktest  <- TRUE
  kcrossvalidation  <- FALSE
  dataset_test  <- dataset[ fold_test== 1 ]
}


#Si hay testing, sin validation,  STOP !!
if( kvalidate== FALSE & ktest== TRUE ) stop("Error, si hay testing, debe haber validation \n") 


rm( dataset )
gc()


#si ya existe el archivo log, traigo hasta donde procese
if( file.exists( "BO_log.txt" ) )
{
  tabla_log  <- fread( "BO_log.txt" )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_ganancia   <- tabla_log[ , max(ganancia) ]
  rm(tabla_log)
} else  {
  GLOBAL_iteracion  <- 0
  GLOBAL_ganancia   <- -Inf
}


#Aqui comienza la configuracion de mlrMBO

#deobo hacer cross validation o  Train/Validate/Test
if( kcrossvalidation ) {
  funcion_optimizar  <- EstimarGanancia_lightgbmCV
} else {
  funcion_optimizar  <- EstimarGanancia_lightgbm
}


configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

#archivo donde se graba y cada cuantos segundos
ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  
                         save.file.path=       "bayesiana.RDATA" )
                         
ctrl  <- setMBOControlTermination( ctrl, 
                                   iters= kBO_iteraciones )   #cantidad de iteraciones
                                   
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km",
                        predict.type= "se",
                        covtype= "matern3_2",
                        control= list(trace= TRUE) )



#Aqui inicio la optimizacion bayesiana
if( !file.exists( "bayesiana.RDATA" ) ) {

  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)

} else {
  #si ya existe el archivo RDATA, debo continuar desde el punto hasta donde llegue
  #  usado para cuando se corta la virtual machine
  run  <- mboContinue( "bayesiana.RDATA" )   #retomo en caso que ya exista
}

