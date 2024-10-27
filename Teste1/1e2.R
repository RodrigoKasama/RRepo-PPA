# Carregar os pacotes necessários
if(!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if(!require(daltoolbox, quietly = TRUE)) install.packages("daltoolbox")
if(!require(gridExtra, quietly = TRUE)) install.packages("gridExtra")
if(!require(ggplot2, quietly = TRUE)) install.packages("ggplot2") # Verifica e instala ggplot2

library(dplyr)
library(daltoolbox)
library(gridExtra)
library(ggplot2) # Carrega ggplot2

# Baixando e lendo o arquivo de dados
data_wines <- read.table('http://www.archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
                         sep = ',', col.names = c('class', 'Alcohol', 'Malicacid', 'Ash', 'Alcalinity_of_ash',
                                                  'Magnesium', 'Total_phenols', 'Flavanoids', 'Nonflavanoid_phenols',
                                                  'Proanthocyanins', 'Color_intensity', 'Hue',
                                                  '0D280_0D315_of_diluted_wines', 'Proline'))

data_wines$class <- as.character(data_wines$class)

### Parte 1: Estatísticas básicas ###

# a. Média e desvio padrão de cada coluna
dw_mean <- data_wines %>% summarise(across(.cols = !class, .fns = mean, na.rm = TRUE))
dw_sd <- data_wines %>% summarise(across(.cols = !class, .fns = sd, na.rm = TRUE))

# b. Média e desvio padrão por classe
dw_mean_grouped <- data_wines %>% group_by(class) %>% summarise(across(.cols = everything(), .fns = mean, na.rm = TRUE))
dw_sd_grouped <- data_wines %>% group_by(class) %>% summarise(across(.cols = everything(), .fns = sd, na.rm = TRUE))

### Parte 2: Visualização ###

## Gráficos de densidade
plots_density <- list()
for(col_name in colnames(data_wines)[-1]) {
  plots_density[[col_name]] <- ggplot(data_wines, aes_string(x = col_name, fill = "class")) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Densidade de", col_name, "por classe de vinho"),
         x = col_name, y = "Densidade") +
    theme_minimal() +
    scale_fill_manual(values = c('#0099e5', '#ff4c4c', '#34bf49'))
}

density_plot_grouped <- grid.arrange(grobs = plots_density, ncol = 3)

## Gráficos de boxplot
plots_boxplot <- list()
for(col_name in colnames(data_wines)[-1]) {
  plots_boxplot[[col_name]] <- ggplot(data_wines, aes_string(x = "class", y = col_name, fill = "class")) +
    geom_boxplot() +
    labs(title = paste("Box-Plot de", col_name, "por classe de vinho"),
         x = "Classe", y = col_name) +
    theme_minimal() +
    scale_fill_manual(values = c('#0099e5', '#ff4c4c', '#34bf49'))
}

boxplot_grouped <- grid.arrange(grobs = plots_boxplot, ncol = 3)

## Gráficos de dispersão entre atributos
plots_scatter <- list()
attrs <- colnames(data_wines)[-1]
len <- 5  # Limitando o número de atributos para visualização
for(i in 1:len) {
  for(j in 1:len) {
    if(i < j) {
      plots_scatter[[paste(attrs[i], attrs[j], sep = ' x ')]] <- ggplot(data_wines, aes_string(x = attrs[i], y = attrs[j], color = "class")) +
        geom_point(alpha = 0.7) +
        labs(title = paste(attrs[i], "vs", attrs[j]), x = attrs[i], y = attrs[j]) +
        theme_minimal() +
        scale_color_manual(values = c('#0099e5', '#ff4c4c', '#34bf49'))
    }
  }
}

scatter_plot_grouped <- grid.arrange(grobs = plots_scatter, ncol = 3)

### questão 2 ###

## a. Discretização dos atributos
dw_discretized <- data_wines %>%
  mutate(across(.cols = !class, .fns = function(x) {
    breaks <- quantile(x, probs = seq(0, 1, by = 1/3), na.rm = TRUE)  # Usar na.rm para lidar com NAs
    labels <- c('Baixo', 'Médio', 'Alto')
    cut(x, breaks, labels, include.lowest = TRUE)
  }))

## b. Mapeamento categórico
cm <- categ_mapping('class')

# Criando o data frame com a classe mapeada
dw_categ_mapping <- cbind(transform(cm, dw_discretized), dw_discretized[-1])

# Visualizar os dados discretizados
View(dw_discretized)
View(dw_categ_mapping)  # Visualizando o mapeamento categórico

# Para evitar possíveis erros de visualização, se você quiser visualizar
# um resumo dos dados discretizados, pode usar:
summary(dw_discretized)
summary(dw_categ_mapping)
