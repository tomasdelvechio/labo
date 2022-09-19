rm(list = ls())
gc(verbose = FALSE)

require("data.table")
require("rpart")
require("ggplot2")
require("randomForest")
require("lightgbm")

wlog <- function(line, file = "./logs/c2-predictor.log", append = TRUE, newrun = FALSE) {
    if (newrun == TRUE) {
        cat(file = "./logs/c2-predictor.log", "=======================", "\n", append = TRUE)
        cat(file = "./logs/c2-predictor.log", "Nueva ejecución de predictor.R:", format(Sys.time(), "%Y%m%d%H%M%S"), "\n", append = TRUE)
    } else {
        cat(file = file, line, "\n", append = append)
    }
}

wlog("", newrun = TRUE)

#cat(file = "./logs/c2-predictor.log", "=======================", "\n", append = TRUE)
#cat(file = "./logs/c2-predictor.log", "Nueva ejecución de predictor.R:", format(Sys.time(), "%Y%m%d%H%M%S"), "\n", append = TRUE)

setwd("/home/tomas/workspace/uba/dmeyf")
semillas  <- c(697157, 585799, 906007, 748301, 372871)
# Mejor semilla del publico 585799

# Cargamos todo para tener un código limpio
dataset <- fread("./datasets/competencia2_2022.csv.gz")
marzo <- dataset[foto_mes == 202101]
mayo <- dataset[foto_mes == 202105]
rm(dataset)

clase_binaria <- ifelse(marzo$clase_ternaria == "BAJA+2", 1, 0)
marzo$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data = data.matrix(marzo), label = clase_binaria)

punto_de_corte <- 0.025

ganancia_lgb <- function(probs, datos) {
  return(list("name" = "ganancia",
                "value" =  sum((probs > punto_de_corte) *
                    ifelse(getinfo(datos, "label") == 1, 78000, -2000)) / 0.2,
                "higher_better" = TRUE))
}


for (semilla in semillas) {
    set.seed(semilla)
    model_lgbm_cv <- lgb.cv(data = dtrain,
            eval = ganancia_lgb,
            stratified = TRUE,
            nfold = 5,
            param = list(objective = "binary",
                        max_bin = 15,
                        min_data_in_leaf = 4000,
                        learning_rate = 0.05,
                        verbose = -1,
                        num_leaves = 20
                        ),
            verbose = -1
        )

    # Mejor iteración
    #model_lgbm_cv$best_iter

    # Ganancia de la mejor iteración
    #unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]
    wlog(paste("Semilla: ", semilla, " - Ganancia Train: ", unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]))

    # Una vez que elegimos los parámetros tenemos que entrenar con todos.
    model_lgm <- lightgbm(data = dtrain,
                nrounds = model_lgbm_cv$best_iter, # <--- OJO! Double Descent alert
                params = list(objective = "binary",
                                max_bin = 15,
                                min_data_in_leaf = 4000,
                                learning_rate = 0.05),
                verbose = -1)

    # También tiene su importancia de variables
    lgb.importance(model_lgm, percentage = TRUE)

    ## ---------------------------
    ## Prediciendo Mayo
    ## ---------------------------

    mayo$pred <- predict(model_lgm, data.matrix(mayo[, 1:154]))
    mayo[, Predicted := as.numeric(pred > punto_de_corte)]

    dir.create("./exp/", showWarnings = FALSE)
    dir.create("./exp/KAGC2", showWarnings = FALSE)

    output_filename <- paste0("output-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", round(runif(1) * 100), ".csv")

    fwrite(mayo[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
        file = paste0("./exp/KAGC2/", output_filename),
        sep = ","
    )

    #cat("Punto de Corte:", punto_de_corte, "\n")
    wlog(paste("Cantidad de Bajas Predichas:", sum(mayo$Predicted)))
    wlog(paste("Resultados para subir a Kaggle:", output_filename))
}