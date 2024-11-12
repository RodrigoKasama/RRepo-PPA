# Carregar a biblioteca necessária
library(dplyr)

# Função para calcular a entropia
calculate_entropy <- function(data) {
  freqs <- table(data) / length(data)
  -sum(freqs * log(freqs + 1e-6))
}

# Carregar e preparar os dados
wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)
colnames(wine_data)[1] <- "Class"

# Removendo target
data_for_model <- wine_data %>% select(-Class)

# Ajustar o modelo de K-means com 3 clusters
modelo <- kmeans(data_for_model, centers = 3)
clusters <- modelo$cluster

# Calcular a entropia para cada cluster
entropy_clusters <- sapply(unique(clusters), function(cluster_id) {
  calculate_entropy(wine_data$Class[clusters == cluster_id])
})

# Calcular a entropia para a classe "Class" original
entropy_type <- calculate_entropy(wine_data$Class)

# Exibir os resultados
cat("Entropia para cada grupo gerado pelo K-means:\n", entropy_clusters, "\n")
cat("Entropia para a classe 'Class' original:\n", entropy_type, "\n")

