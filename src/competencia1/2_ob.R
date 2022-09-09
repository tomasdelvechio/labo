setwd("/home/tomas/workspace/uba/dmeyf")

df_parameters <- as.data.frame(
    # c("xval", "cp", "minsplit", "minbucket", "maxdepth")
    t(c(    0 ,  -1 ,       192 ,        310 ,        10))
)
colnames(df_parameters) <- c("xval", "cp", "minsplit", "minbucket", "maxdepth")

write.table(df_parameters, #$Value, 
    #row.names = df_parameters,
    #col.names = FALSE,
    sep = ";",
    "./datasets/rpart_parameters.csv")
