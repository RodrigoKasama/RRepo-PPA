library(dplyr)
library(tidyr)
library(cluster)

# Lendo os dados
wines <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data', sep = ',', col.names = c('Type', 'Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Magnesium', 'Phenols', 'Flavanoids', 'Nonflavanoids', 'Proanthocyanins', 'Color', 'Hue', 'Dilution', 'Proline'))

# Ajustando o modelo de K-means
modelo <- cluster_kmeans(k = 3)

# Selecionando todas as colunas exceto "Type" para o modelo
data_for_model <- select(wines, -Type)

# Ajustando o modelo com todas as colunas exceto "Type"
modelo <- fit(modelo, data_for_model)

# Obtendo os clusters
clu <- cluster(modelo, data_for_model)

# Função para calcular a entropia
calculate_entropy <- function(data) {
  freqs <- table(data) / length(data)
  -sum(freqs * log(freqs + 1e-6))
}

# Calculando a entropia para cada grupo gerado pelo K-means
entropy_clusters <- sapply(unique(clu), function(cluster_id) {
  data_in_cluster <- wines$Type[clu == cluster_id]
  calculate_entropy(data_in_cluster)
})

# Calculando a entropia para a classe "Type" original
entropy_type <- calculate_entropy(wines$Type)

# Exibindo a entropia para cada grupo gerado pelo K-means
print("Entropia para cada grupo gerado pelo K-means:")
print(entropy_clusters)

# Exibindo a entropia para a classe "Type" original
print("Entropia para a classe 'Type' original:")
print(entropy_type)