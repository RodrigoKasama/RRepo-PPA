setwd("/home/lucas_silva/RRepo-PPA/Teste1")
# Carregar as bibliotecas necessárias
library(ggplot2)
library(dplyr)

# Lendo o arquivo CSV sem cabeçalho
vinhos <- read.csv("vinhos.csv", header = FALSE, sep = ",")

# Definindo o nome da primeira coluna como tipo de vinho
colnames(vinhos)[1] <- "Tipo"

# Convertendo a coluna Tipo em um fator e definindo a ordem
vinhos$Tipo <- factor(vinhos$Tipo, levels = sort(unique(vinhos$Tipo)))

# Criando uma pasta para salvar os gráficos, se não existir
if (!dir.exists("boxplot_1D")) {
  dir.create("boxplot_1D")
}

# Criando um gráfico box-plot para cada atributo, agrupado pelo tipo de vinho
for (i in 2:ncol(vinhos)) {
  p <- ggplot(vinhos, aes_string(x = "Tipo", y = colnames(vinhos)[i], fill = "Tipo")) +
    geom_boxplot() +
    labs(title = paste("Box-Plot para", colnames(vinhos)[i]),
         x = "Tipo de Vinho",
         y = colnames(vinhos)[i]) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Salvando cada gráfico na pasta boxplot_1D
  ggsave(filename = paste0("boxplot_1D/boxplot_", colnames(vinhos)[i], ".png"), plot = p)
}
