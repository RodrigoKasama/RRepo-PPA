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
dw_mean_grouped = wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = mean))

dw_sd_grouped = wine_data %>%
  group_by(Class) %>%
  summarise(across(.cols = everything(), .fns = sd))


#Q1 - c

plots = list()
for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_density_class(wine_data %>% select(Class, col_name), class_label = 'Class', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'), 
                                         alpha = .7)
}

density_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

# Q1 - d

plots = list()
for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_boxplot_class(wine_data %>% select(Class, col_name), 
                                         class_label = 'Class', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'))
}

boxplot_grouped = grid.arrange(grobs = plots, ncol = 5)

#Q1 - e
plots = list()
len = 5
attrs = colnames(wine_data)[-1]
for(i in 1:len) {
  for(j in 1:len) {
    if(i < j) {
      plots[[paste(attrs[i], attrs[j], sep = 'x')]] = plot_scatter(wine_data %>% select(x = attrs[i], value = attrs[j], variable = Class),
                                                                   label_x = attrs[i], label_y = attrs[j], colors = c("red", "green", "blue"))
    }
  }
}

scatter_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

