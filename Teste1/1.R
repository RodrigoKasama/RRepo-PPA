# Definir o diretório de trabalho
setwd("/home/lucas_silva/RRepo-PPA/Teste1")

# Carregar as bibliotecas necessárias
library(ggplot2)
library(dplyr)

# Lendo o arquivo CSV
vinhos <- read.csv("vinhos.csv", header = TRUE, sep = ",")
head(vinhos)

# Q1 - a: Cálculo da média e desvio padrão de todas as colunas
media_vinhos <- colMeans(vinhos, na.rm = TRUE)
desvio_padrao_vinhos <- sapply(vinhos, sd, na.rm = TRUE)

# Exibindo média e desvio padrão
media_vinhos
desvio_padrao_vinhos

# Q1 - b: Média e desvio padrão por tipo de vinho
colnames(vinhos)[1] <- "Tipo_Vinho"
vinhos$Tipo_Vinho <- as.factor(vinhos$Tipo_Vinho)  # Garantir que Tipo_Vinho é um fator
media_vinhos <- aggregate(. ~ Tipo_Vinho, data = vinhos, FUN = mean, na.rm = TRUE)
desvio_padrao_vinhos <- aggregate(. ~ Tipo_Vinho, data = vinhos, FUN = sd, na.rm = TRUE)

# Exibindo média e desvio padrão por tipo de vinho
media_vinhos
desvio_padrao_vinhos

# Q1 - c: Criando a pasta para salvar os gráficos de densidade, se não existir
if (!dir.exists("graficos_1C")) {
  dir.create("graficos_1C")
}

# Criando e salvando os gráficos de densidade
for (i in 2:ncol(vinhos)) {
  p <- ggplot(vinhos, aes(x = vinhos[[i]], fill = Tipo_Vinho)) +
    geom_density(alpha = 0.5, position = "identity") +
    labs(title = paste("Distribuição de Densidade para", colnames(vinhos)[i]),
         x = colnames(vinhos)[i],
         y = "Densidade") +
    theme_minimal() +
    scale_fill_manual(values = c("#0099e5", "#ff4c4c", "#34bf49"))  # Definindo cores específicas
  
  # Salvando cada gráfico na pasta graficos_1C
  ggsave(filename = paste0("graficos_1C/densidade_", colnames(vinhos)[i], ".png"), plot = p)
}

# Q1 - d: Criando a pasta para salvar os gráficos de box-plot, se não existir
if (!dir.exists("boxplot_1D")) {
  dir.create("boxplot_1D")
}

# Criando e salvando gráficos box-plot para cada atributo, agrupado pelo tipo de vinho
for (i in 2:ncol(vinhos)) {
  p <- ggplot(vinhos, aes_string(x = "Tipo_Vinho", y = colnames(vinhos)[i], fill = "Tipo_Vinho")) +
    geom_boxplot(outlier.size = 2, outlier.colour = "red") +  # Destacando outliers
    labs(title = paste("Box-Plot para", colnames(vinhos)[i]),
         x = "Tipo de Vinho",
         y = colnames(vinhos)[i]) +
    theme_minimal() +
    scale_fill_manual(values = c("#0099e5", "#ff4c4c", "#34bf49"))  # Definindo cores específicas
  
  # Salvando cada gráfico na pasta boxplot_1D
  ggsave(filename = paste0("boxplot_1D/boxplot_", colnames(vinhos)[i], ".png"), plot = p)
}

# Q1 - e: Criando a pasta para salvar os gráficos de dispersão, se não existir
if (!dir.exists("dispersao_1E")) {
  dir.create("dispersao_1E")
}

# Criando gráficos de dispersão para cada par de atributos
for (i in 2:(ncol(vinhos) - 1)) {
  for (j in (i + 1):ncol(vinhos)) {
    p <- ggplot(vinhos, aes_string(x = colnames(vinhos)[i], y = colnames(vinhos)[j], color = "Tipo_Vinho")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Adicionando linha de tendência
      labs(title = paste("Gráfico de Dispersão entre", colnames(vinhos)[i], "e", colnames(vinhos)[j]),
           x = colnames(vinhos)[i],
           y = colnames(vinhos)[j]) +
      theme_minimal() +
      scale_color_manual(values = c("#0099e5", "#ff4c4c", "#34bf49"))  # Definindo cores específicas
    
    # Salvando cada gráfico na pasta dispersao_1E
    ggsave(filename = paste0("dispersao_1E/dispersao_", colnames(vinhos)[i], "_vs_", colnames(vinhos)[j], ".png"), plot = p)
  }
}
