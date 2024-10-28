# Carregar a biblioteca necessária
library(dplyr)

# Função para calcular a entropia
calculate_entropy <- function(data) {
  freqs <- table(data) / length(data)
  -sum(freqs * log(freqs + 1e-6))
}

# Carregar e preparar os dados
wines <- read.table(
  'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
  sep = ',',
  col.names = c('Type', 'Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Magnesium', 
                'Phenols', 'Flavanoids', 'Nonflavanoids', 'Proanthocyanins', 
                'Color', 'Hue', 'Dilution', 'Proline')
)
data_for_model <- wines %>% select(-Type)

# Ajustar o modelo de K-means com 3 clusters
modelo <- kmeans(data_for_model, centers = 3)
clusters <- modelo$cluster

# Calcular a entropia para cada cluster
entropy_clusters <- sapply(unique(clusters), function(cluster_id) {
  calculate_entropy(wines$Type[clusters == cluster_id])
})

# Calcular a entropia para a classe "Type" original
entropy_type <- calculate_entropy(wines$Type)

# Exibir os resultados
cat("Entropia para cada grupo gerado pelo K-means:\n", entropy_clusters, "\n")
cat("Entropia para a classe 'Type' original:\n", entropy_type, "\n")

