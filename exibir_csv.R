### etapa inicial ###

wine_data = read.table('http://www.archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
                        sep = ',', col.names = c('class', 'Alcohol', 'Malicacid', 'Ash', 'Alcalinity_of_ash', 'Magnesium', 'Total_phenols', 'Flavanoids', 'Nonflavanoid_phenols', 'Proanthocyanins', 'Color_intensity', 'Hue', '0D280_0D315_of_diluted_wines', 'Proline'))
colnames(wine_data)[1] <- "Class"


### questão 1 ###

## a ##

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
## Q1 - c ##

plots = list()

for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_density_class(wine_data %>% select(Class, col_name), 'class', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'), 
                                         alpha = .7)
}

density_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

rm(plots)
