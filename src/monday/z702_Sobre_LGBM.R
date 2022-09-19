##
## Sobre LGBM
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## Light up the darkness.
## --- Bob Marley

# Bajaremos a detalle como funciona el lightGBM a medida que recorremos sus parámetros

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf2022")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
marzo <- dataset[foto_mes == 202103]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data   = data.matrix(marzo),
                       label  = clase_binaria,
                       # Truco jedi!
                       weight = ifelse(clase_real == "BAJA+2", 1.0000001, 1.0))

# Veremos en detalle esta función un poco más adelante
ganancia_lgbm  <- function(probs, datos) {
    ## Ingresar su estrategia! acá vamos a ir simplemente buscando la máxima gan de la curva.
    gan <- data.table("pred" = probs,
                                                                 # truco para separar las clases
                    "gan" = ifelse(getinfo(datos, "label") == 1 & getinfo(datos, "weight") > 1, 78000, -2000))
    setorder(gan, -pred)
    gan[, gan_acum :=  cumsum(gan)]
    return(list("name" = "ganancia",
                    "value" = gan[, max(gan_acum)] / 0.2,
                    "higher_better" = TRUE))
}

## Sobre el LightGBM
## Es esta materia no somos muy fan del canal StatQuest. Pero! Realmente las series de
## Gradient Boosting: https://www.youtube.com/watch?v=3CC4N4z3GJc&list=PLblh5JKOoLUJjeXUvUE0maghNuY2_5fY6
## XGBoost: https://www.youtube.com/watch?v=OtD8wVaFm6E&list=PLblh5JKOoLULU0irPgs1SnKO6wqVjKUsQ
## Son 4 horas que valen la pena para complementar la base dada en clase.
## También vale la pena profundizar el en link:boost_from_average
## https://lightgbm.readthedocs.io/en/v3.3.2/Features.html

set.seed(semillas[1])

model_lgbm_cv <- lgb.cv(

    # Configuración del CV:
    data = dtrain,
    nfold = 5,
    stratified = TRUE,

    # Función que va a ser evaluada.
    eval = ganancia_lgbm,

    # Veremos algunos, pero hay muchos más https://lightgbm.readthedocs.io/en/v3.3.2/Parameters.html
    param = list(
        # Seteamos la semilla. Se puede setear semillar para cada uso de parámetro aleatorio.
        # - bagging_seed
        # - feature_fraction_seed
        seed= semillas[1],

        # Definimos que tipo de problema vamos a resolver. De esta forma sabe que loss function utilizar.
        objective = "binary",
        # Que función va a usar para evaluar, en este caso es la que le pasamos en **eval**
        # cuenta con una gran cantidad de funciones por out-of-the-box.
        metric = "custom",

        # Tipo de boosting: Usamos el valor por defecto, pero cuenta con más tipos: gbdt, rf, dart, goss
        boosting = "gbdt",

        # Valores del primer árbol usando el valor de la media de las clases. Ahorra quizás, 3 árboles (?)
        boost_from_average = TRUE,

        # Un parámetro más que importante! LightGBM bindea automaticamente las variables
        # Hace que las variables pesen menos en memoria
        # Hace más rápido en su ejecución
        # Hace más robusto la predicción
        max_bin = 31,

        # Por default puede trabajar con missing. Pero siempre hay un alumno talibán.
        use_missing = TRUE,

        # Variables de crecimiento del árbol.
        max_depth = 12, # -1 = No limitar
        min_data_in_leaf = 4000,
        feature_pre_filter = FALSE, #feature_pre_filter: Evita que LightGBM deje de lado variables que considera malas.
        num_leaves = 100,

        # Parámetros que fueron sacados de los rf porque lo que anda se mete:
        feature_fraction = 0.50, # Porcentaje de columnas que van a usarse en un árbol
        # feature_fraction_bynode si queremos que sea por nodo
        bagging_fraction = 1.0, # % de registros a considerar en cada árbol
        extra_tree = FALSE, # Los puntos de corte los elige al azar.

        # Parámetros de las famosas regularizaciones!!
        lambda_l1 = 0.0,
        lambda_l2 = 0.0,
        min_gain_to_split = 0.0,

        # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
        learning_rate =  0.01,

        # Cuántos árboles vamos a generar
        num_iterations = 100, # Debe ser un número muy grande, recordar el double descent!!!.
        early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.
    ),
    verbose = -1
)

model_lgbm_cv$best_iter
model_lgbm_cv$best_score

model_lgbm_cv$record_evals

# Armamos el modelo:
model_lgm <- lightgbm(
    data = dtrain,
    params = list(
        seed = semillas[1],
        objective = "binary",
        boost_from_average = TRUE,
        max_bin = 31,
        max_depth = 12,
        min_data_in_leaf = 4000,
        feature_pre_filter = FALSE,
        num_leaves = 100,
        feature_fraction = 0.50,
        learning_rate = 0.01,
        num_iterations = model_lgbm_cv$best_iter
    ),
    verbose = -1
)

lgb.importance(model_lgm)

## ACTIVIDAD para la clase: Armar una Opt. Bayesiana para el LightGBM.
## Empezaran a recibir cada vez más soporte de código, algo que en la vida de
## a deveras no va a pasar. Mis valientes C1 demustren estar preparados para la
## calle haciendo su propia Opt Bay.