# Definir o diretório de trabalho
setwd("/home/lucas_silva/RRepo-PPA/Teste1")

# Carregar as bibliotecas necessárias
library(dplyr)

# Lendo o arquivo CSV
vinhos <- read.csv("vinhos.csv", header = TRUE, sep = ",")

# Verificando os dados
head(vinhos)

# Renomeando a primeira coluna para Tipo_Vinho (se ainda não estiver feito)
colnames(vinhos)[1] <- "Tipo_Vinho"

# Q2 - a: Discretização dos atributos numéricos em faixas de valores (exceto a classe)
# A primeira coluna é do tipo de vinho, portanto, vamos aplicar a discretização nas colunas 2 até n
for (i in 2:ncol(vinhos)) {
  # Discretizando os atributos em faixas 'alto', 'médio' e 'baixo'
  vinhos[[i]] <- cut(vinhos[[i]], 
                     breaks = quantile(vinhos[[i]], probs = seq(0, 1, by = 1/3), na.rm = TRUE),
                     labels = c("Baixo", "Médio", "Alto"), 
                     include.lowest = TRUE)
}

# Verificando a transformação
head(vinhos)

# Q2 - b: Convertendo o atributo do tipo de vinho para mapeamento categórico
vinhos$Tipo_Vinho <- as.factor(vinhos$Tipo_Vinho)  # Convertendo para fator

# Verificando as mudanças
str(vinhos)
