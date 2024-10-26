vinhos <- read.csv("vinhos.csv", header = FALSE, sep = ",")
colnames(vinhos)[1] <- "Tipo_Vinho"


media_vinhos <- aggregate(. ~ Tipo_Vinho, data = vinhos, FUN = mean, na.rm = TRUE)
desvio_padrao_vinhos <- aggregate(. ~ Tipo_Vinho, data = vinhos, FUN = sd, na.rm = TRUE)

media_vinhos
desvio_padrao_vinhos
