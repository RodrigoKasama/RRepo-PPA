setwd("/home/lucas_silva/RRepo-PPA/Teste1")
vinhos <- read.csv("vinhos.csv", header = FALSE, sep = ",")
head(vinhos)

media_vinhos <- colMeans(vinhos, na.rm = TRUE)

desvio_padrao_vinhos <- sapply(vinhos, sd, na.rm = TRUE)

media_vinhos
desvio_padrao_vinhos
