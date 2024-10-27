# Carregar os pacotes necessários
library(dplyr)

# Lendo o arquivo CSV com cabeçalho
vinhos <- read.csv("vinhos.csv", header = TRUE, sep = ",")

# Convertendo a coluna Tipo em um fator
vinhos$Type <- as.factor(vinhos$Type)

# Discretizando os atributos numéricos (exceto a classe)
for (i in 2:ncol(vinhos)) {
  vinhos[[i]] <- cut(vinhos[[i]], 
                     breaks = quantile(vinhos[[i]], probs = c(0, 0.33, 0.67, 1)), 
                     labels = c("baixo", "medio", "alto"), 
                     include.lowest = TRUE)
}

# Confirmando as mudanças
str(vinhos)
