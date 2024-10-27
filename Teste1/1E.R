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

# Criando a pasta para salvar os gráficos de dispersão, se não existir
if (!dir.exists("dispersao_1E")) {
  dir.create("dispersao_1E")
}

# Criando gráficos de dispersão para cada par de atributos
for (i in 2:(ncol(vinhos) - 1)) {
  for (j in (i + 1):ncol(vinhos)) {
    p <- ggplot(vinhos, aes_string(x = colnames(vinhos)[i], y = colnames(vinhos)[j], color = "Tipo")) +
      geom_point(alpha = 0.6) +
      labs(title = paste("Gráfico de Dispersão entre", colnames(vinhos)[i], "e", colnames(vinhos)[j]),
           x = colnames(vinhos)[i],
           y = colnames(vinhos)[j]) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    # Salvando cada gráfico na pasta dispersao_1E
    ggsave(filename = paste0("dispersao_1E/dispersao_", colnames(vinhos)[i], "_vs_", colnames(vinhos)[j], ".png"), plot = p)
  }
}
