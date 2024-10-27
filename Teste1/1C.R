setwd("/home/lucas_silva/RRepo-PPA/Teste1")
library(ggplot2)
library(dplyr)

# Lendo o arquivo CSV sem cabeçalho
vinhos <- read.csv("vinhos.csv", header = TRUE, sep = ",")

# Definindo o nome da primeira coluna como tipo de vinho
colnames(vinhos)[1] <- "Tipo"

# Convertendo a coluna Tipo em um fator e definindo a ordem
vinhos$Tipo <- factor(vinhos$Tipo, levels = sort(unique(vinhos$Tipo)))

# Criando a pasta para salvar os gráficos, se não existir
if (!dir.exists("graficos_1C")) {
  dir.create("graficos_1C")
}

# Criando e salvando os gráficos de densidade
for (i in 2:ncol(vinhos)) {
  p <- ggplot(vinhos, aes(x = vinhos[[i]], fill = Tipo)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribuição de Densidade para", colnames(vinhos)[i]),
         x = colnames(vinhos)[i],
         y = "Densidade") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Salvando cada gráfico na pasta graficos_1C
  ggsave(filename = paste0("graficos_1C/densidade_", colnames(vinhos)[i], ".png"), plot = p)
}

