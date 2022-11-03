#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("Matrix")
require("xgboost")
require("SHAPforxgboost")
require("shapr")


#Parametros del script
PARAM <- list()
PARAM$experimento  <- "IMP1202"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/")
#cargo el dataset
dataset <- fread( "./datasets/competencia3_2022.csv.gz")

dataset  <- dataset[ foto_mes %in% c(202103, 202105) ]
gc()

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#agrego a mis fieles canaritos
# nada temo porque Ellos son mis centinelas y delataran a los embusteros
for( i in 1:20 )  dataset[ , paste0( "canarito", i ) := runif(nrow(dataset)) ]

#debo poner la clase en {0,1}
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+2"), 1, 0) ]


col_buenas  <- setdiff(colnames(dataset) ,c("clase_ternaria","clase01") )


#genero el dataset en el formato interno que necesita gradient voosting
dtrain <- xgb.DMatrix( data=  as.matrix(dataset[ foto_mes==202103, col_buenas, with=FALSE]),
                       label= dataset[ foto_mes==202103, clase01 ]
                     )


#Entreno con hiperparametros que calcule en una Optimizacion Bayesiana PREVIA
param_best <- list(nrounds= 265, max_leaves= 10, eta= 0.04, colsample_bytree= 0.43, gamma= 4.3)

modelo_final  <- xgboost(data= dtrain,
                         objective= "binary:logistic",
                         nrounds= param_best$nrounds,
                         base_score= mean(getinfo(dtrain, "label")),
                         max_bin= 31,
                         tree_method= "hist",
                         grow_policy= "lossguide",
                         max_leaves= param_best$max_leaves,
                         eta= param_best$eta,
                         colsample_bytree= param_best$colsample_bytree,
                         gamma=  param_best$gamma
                        )

#Calculo la importancia de variables  TRADICIONAL
tb_importancia  <- xgboost::xgb.importance( model= modelo_final )


dapply  <- dataset[ foto_mes==202105 ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo_final,
                        data.matrix( dapply[ , col_buenas, with=FALSE] )  )


#valores Shapley de CADA registro
# devuelve una matrix
shap_values  <- shap.values(xgb_model = modelo_final, 
                            X_train = as.matrix( dapply[ , col_buenas, with=FALSE]))


bias  <-  shap_values$BIAS0[[1]]
nrow( shap_values$shap_score )
ncol( shap_values$shap_score )

#me fijo en el primer registro
shap_values$shap_score[ 1, ]
#calculo el score del primer registros
primero_score  <- sum(shap_values$shap_score[1, ]) + shap_values$BIAS[[1]]

#utillizando la funcion logistica, paso de scrore a probabilidad
primero_prob  <- 1/ ( 1 + exp( - primero_score) )

#inprimo las probabilidades
cat( "Probabilidades del primer registro, dos calculos: ", primero_prob,  prediccion[1], "\n" )
#bastante parecidos ...  jaja



#El registro con mas prob de BAJA+2,  que justo es un BAJA+1 ...
dt  <- shap_values$shap_score[ 38187,]
dt2  <- data.table( names(dt), transpose( dt ) )
colnames( dt2 )  <- c("columna","Shapley" )
dt2[ , Shapley_abs := abs( Shapley ) ]
setorder( dt2, -Shapley_abs )
dt2



dt_mean  <- shap_values$shap_score[, lapply(.SD,function(x) mean(abs(x)) ),]
dt_mean2  <-  data.table( names(dt_mean), transpose( dt_mean ) )
colnames( dt_mean2 )  <-  c("columna","Shapley_mean" )
dt_mean2[ , Shapley_abs := abs( Shapley_mean ) ]
setorder( dt_mean2, -Shapley_abs )

dt_mean2[   1:100]
dt_mean2[ 100:175]


pdf( "shap_xgb_00.pdf", width=20, height=10 )
xgboost::xgb.plot.shap(data= as.matrix(dapply[ 1:2, col_buenas, with=FALSE]), 
                       model= modelo_final, top_n = 2, n_col = 2)
dev.off()


dapply[ , azar := runif( nrow(dapply) ) ]

pdf( "shap_xgb_01.pdf", width=10, height=30 )
xgboost::xgb.plot.shap(data= as.matrix(dapply[  clase01==1 | azar< 0.01, col_buenas, with=FALSE]), 
                       model= modelo_final, top_n = 2, n_col = 2)
dev.off()


#valores Shapley de CADA registro
shap_values <- shap.values(xgb_model = modelo_final, 
                           X_train = as.matrix(dapply[  clase01==1 | azar< 0.01, col_buenas, with=FALSE]))


shap_long <- shap.prep(xgb_model = modelo_final,
                       X_train = as.matrix(dapply[ ( clase01==1 | azar< 0.01), col_buenas, with=FALSE]))


pdf( "shap01.pdf", width=10, height=30 )
shap.plot.summary(shap_long, scientific = TRUE)
dev.off()



#grafico las interacciones
shap_int  <- predict(modelo_final, 
                     as.matrix(dapply[ (clase01==1 | azar< 0.01),  col_buenas, with=FALSE]), 
                     predinteraction = TRUE)

# **SHAP interaction effect plot **
pdf( "shap_inter01.pdf", width=8 )

shap.plot.dependence(data_long= shap_long,
                     data_int= shap_int,
                     x= "mpayroll",
                     y= "cliente_edad", 
                     color_feature= "cliente_edad")
dev.off()


#Force PLOT
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)

# choose to zoom in at location 500, set y-axis limit using `y_parent_limit`  
# it is also possible to set y-axis limit for zoom-in part alone using `y_zoomin_limit`  
pdf( "shap_force01.pdf", width=8 )
shap.plot.force_plot(plot_data, zoom_in_location = 10, y_parent_limit = c(-1,1))
dev.off()

# plot by each cluster
pdf( "shap_force02.pdf", width=8 )
shap.plot.force_plot_bygroup(plot_data)
dev.off()

