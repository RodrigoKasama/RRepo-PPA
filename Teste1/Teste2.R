library(gridExtra)
library(dplyr)
library(daltoolbox)

# Carregar dados
setwd("/home/lucas_silva/RRepo-PPA/Teste1")
vinhos <- read.csv("vinhos.csv", header = TRUE, sep = ",")
vinhos$Type <- as.factor(vinhos$Type)

# Exibir algumas linhas do dataset
head(wine_data[c(1:2, 61:62, 131:132),])

### Q1 - Estatísticas e Visualizações ###

# Q1 - a
stats <- t(data.frame(
  Mean = sapply(wine_data[-1], mean),
  SD = sapply(wine_data[-1], sd)
))

# Q1 - b
dw_mean_grouped = wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = mean))

dw_sd_grouped = wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = sd))

# Q1 - c: Gráficos de densidade
plots = list()
for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_density_class(wine_data %>% select(Class, col_name), 
                                         class_label = 'Class', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'), 
                                         alpha = .7)
}
density_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

# Q1 - d: Gráficos de boxplot
plots = list()
for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_boxplot_class(wine_data %>% select(Class, col_name), 
                                         class_label = 'Class', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'))
}
boxplot_grouped = grid.arrange(grobs = plots, ncol = 5)

# Q1 - e: Gráficos de dispersão
plots = list()
len = 5
attrs = colnames(wine_data)[-1]
for(i in 1:len) {
  for(j in 1:len) {
    if(i < j) {
      plots[[paste(attrs[i], attrs[j], sep = ' x ')]]
      = plot_scatter(wine_data %>% select(x = attrs[i], value = attrs[j], variable = Class),
                     label_x = attrs[i], label_y = attrs[j], 
                     colors = c("red", "green", "blue"))
    }
  }
}
# scatter_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

### Q2 - Discretização dos Atributos ###

# Q2 - a: Discretização
dw_discretized = wine_data %>%
  mutate(across(.cols = !(Class), .fns = function(x) {
    breaks = quantile(x, probs = seq(0, 1, by = 1/3), na.rm = TRUE)  # Usar na.rm para lidar com NAs
    labels = c('Baixo', 'Médio', 'Alto')
    cut(x, breaks, labels, include.lowest = TRUE)
  }))

# Q2 - b: Mapeamento categórico
cm = categ_mapping('Class')  # Usando 'Class' conforme renomeado

dw_categ_mapping = cbind(transform(cm, dw_discretized), dw_discretized[-1])

# Visualizar os dados discretizados
View(dw_discretized)
View(dw_categ_mapping)  # Visualizando o mapeamento categórico
