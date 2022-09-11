rm(list = ls())
gc(verbose = FALSE)

require("data.table")
require("rpart")
require("mlrMBO")
require("mlr")

setwd("/home/tomas/workspace/uba/dmeyf")

semillas <- c(697157, 585799, 906007, 748301, 372871)

dataset <- fread("./datasets/competencia1_2022.csv")
dataset <- dataset[foto_mes == 202101]

# Default params, because yes
df_parameters <- as.data.frame(
    # c("xval", "cp", "minsplit", "minbucket", "maxdepth")
    t(c(    0 ,  -1 ,       192 ,        310 ,        10)) # nolint
)
colnames(df_parameters) <- c("xval", "cp", "minsplit", "minbucket", "maxdepth")

# Optimización bayesiana con libreria MBO

ganancia <- function(probabilidades, clase, punto_de_corte = 0.025) {
    cat("Función ganancia:", sum(
        (probabilidades >= punto_de_corte) * ifelse(clase == "evento", 78000, -2000)
    ), "\n")
    return(sum(
        (probabilidades >= punto_de_corte) * ifelse(clase == "evento", 78000, -2000)
    ))
}

modelo_rpart_ganancia <- function(train, test, cp = -1, ms = 20, mb = 1, md = 10, punto_de_corte = 0.025) {
    modelo <- rpart(clase_binaria ~ .,
        data = train,
        xval = 0,
        cp = cp,
        minsplit = ms,
        minbucket = mb,
        maxdepth = md
    )

    test_prediccion <- predict(modelo, test, type = "prob")
    ganancia(test_prediccion[, "evento"], test$clase_binaria, punto_de_corte = punto_de_corte) / 0.3
}

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10, punto_de_corte = 0.025) {
    gan <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(ds$clase_binaria,
            p = 0.70,
            list = FALSE
        )
        train <- ds[in_training, ]
        test <- ds[-in_training, ]
        # train_sample <- tomar_muestra(train)
        r <- modelo_rpart_ganancia(train, test,
            cp = cp, ms = ms, mb = mb, md = md, punto_de_corte = punto_de_corte
        )
        gan <- c(gan, r)
    }
    mean(gan)
}

obj_fun_md_ms_mb <- function(x, dataset, semillas) {
    experimento_rpart_completo(dataset, semillas,
        md = x$maxdepth,
        ms = x$minsplit,
        mb = floor(x$minsplit * x$minbucket),
        punto_de_corte = x$punto_corte
    )
}

obj_fun <- makeSingleObjectiveFunction(
    minimize = FALSE,
    fn = obj_fun_md_ms_mb,
    par.set = makeParamSet(
        makeIntegerParam("maxdepth", lower = 2L, upper = 30L),
        makeIntegerParam("minsplit", lower = 1L, upper = 2000L),
        makeNumericParam("minbucket", lower = 0, upper = 1),
        makeNumericParam("punto_corte", lower = 0.025, upper = 0.07)
        # makeNumericParam("cp",  lower = -1, upper = 1L)
        # makeNumericParam <- para parámetros continuos
    ),
    noisy = TRUE,
    has.simple.signature = FALSE
)

optimizacion_bayesiana <- function(dataset, semillas) {
    set.seed(semillas[1])

    ctrl <- makeMBOControl()
    ctrl <- setMBOControlTermination(ctrl, iters = 150L)
    ctrl <- setMBOControlInfill(
        ctrl,
        crit = makeMBOInfillCritEI(),
        opt = "focussearch",
        # sacar parámetro opt.focussearch.points en próximas ejecuciones
        #opt.focussearch.points = 20
    )

    surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

    return(mbo(obj_fun, learner = surr_km, control = ctrl, more.args = list(dataset = dataset, semillas = semillas), show.info = TRUE))
}

if (TRUE) {
    mbo_res <- optimizacion_bayesiana(dataset, semillas)
}

df_parameters <- as.data.frame(mbo_res$opt.path)
# Falta automatizar esto, agarra los primeros 5 o 6 resultados
print(head(df_parameters[order(df_parameters$y, decreasing = TRUE), ]))

#write.table(df_parameters, #$Value, 
#    #row.names = df_parameters,
#    #col.names = FALSE,
#    sep = ";",
#    "./datasets/rpart_parameters.csv")
