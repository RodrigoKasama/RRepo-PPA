library(gridExtra)
wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)

colnames(wine_data)[1] <- "Class"
head(wine_data[c(1:2, 61:62, 131:132),])

# Q1 - a
stats <- t(data.frame(
  Mean = sapply(wine_data[-1], mean),
  SD = sapply(wine_data[-1], sd)
))


# Q1 - b
dw_mean_grouped <- wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = mean, .names = "Mean_{.col}"))

dw_sd_grouped <- wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = sd, .names = "SD-{.col}"))

dw_stats_grouped <- t(left_join(dw_mean_grouped, dw_sd_grouped, by = "Class"))
print(dw_stats_grouped)

# Definindo parâmetros fixos
class_label <- 'Class'
colors <- c('#0099e5', '#34bf49', '#ff4c4c')
alpha <- 0.7

#Q1 - c

plots <- list()
# Iterando por cada coluna
plots <- lapply(colnames(wine_data)[-1], function(col_name) {
  plot_density_class(
    wine_data[, c('Class', col_name), drop = FALSE], 
    class_label = class_label, 
    label_x = paste(col_name, 'by wine class'), 
    colors = colors, 
    alpha = alpha
  )
})

# Gerando uma grid de graficos
density_plot_grouped <- grid.arrange(grobs = plots, ncol = 5)

# Q1 - d

plots <- list()
plots <- lapply(colnames(wine_data)[-1], function(col_name) {
  plot_boxplot_class(
    wine_data[, c('Class', col_name), drop = FALSE], 
    class_label = class_label, 
    label_x = paste(col_name, 'by wine class'), 
    colors = colors
  )
})

boxplot_grouped <- grid.arrange(grobs = plots, ncol = 5)

# Q1 - e
plots <- list()
# Obtenção das combinações de pares de colunas
attrs <- colnames(wine_data)[-1]
combs <- combn(attrs, 2, simplify = FALSE)
max_plots <- 12

combs <- combs[1:min(length(combs), max_plots)]

# Gerando os gráficos com lapply
plots <- lapply(combs, function(pair) {
  plot_scatter(
    wine_data %>% select(x = pair[1], value = pair[2], variable = Class),
    label_x = pair[1],
    label_y = pair[2],
    colors = colors
  )
})

# Nomeando os gráficos na lista com base nos pares de atributos
names(plots) <- sapply(combs, function(pair) paste(pair, collapse = 'x'))

# Organizando os gráficos em uma grade
scatter_plot_grouped <- grid.arrange(grobs = plots, ncol = 5)


