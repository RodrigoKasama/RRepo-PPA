# Carregar bibliotecas necessárias
# Instalar devtools se ainda não tiver
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Instalar DAL do GitHub
devtools::install_github("universidade-do-algarve/DAL")

library(dal)

# 1. Pré-processamento: prepare seus dados
# Certifique-se de que a coluna de tipo de vinho está no formato correto
vinhos$Tipo_Vinho <- as.factor(vinhos$Tipo_Vinho)

# 2. Normalizar os dados
# Excluir a coluna de classe (Tipo_Vinho) e normalizar as colunas restantes
dados_normalizados <- scale(vinhos[, -1])  # Excluindo a primeira coluna (Tipo_Vinho)

# 3. Aplicar o K-means
set.seed(123)  # Para reprodutibilidade
k <- 3  # Número de clusters, ajuste conforme necessário
kmeans_model <- kmeans(dados_normalizados, centers = k)

# 4. Adicionar os rótulos dos clusters ao dataframe original
vinhos$Cluster <- as.factor(kmeans_model$cluster)

# 5. Calcular a entropia de cada grupo
# Função para calcular a entropia
calcular_entropia <- function(grupo, classe) {
  tabela_contingencia <- table(grupo, classe)
  probabilidades <- prop.table(tabela_contingencia, 1)
  entropia <- -sum(probabilidades * log(probabilidades + 1e-10), na.rm = TRUE)  # Evita log(0)
  return(entropia)
}

# Calcular a entropia para cada cluster
entropias <- tapply(vinhos$Tipo_Vinho, vinhos$Cluster, calcular_entropia)
entropias
