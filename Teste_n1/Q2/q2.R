library(dplyr)
wine_dataset_path <- "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)
colnames(wine_data)[1] <- "Class"
### questao 2 ###

#letra a
wine_data_discretized <- wine_data %>%
  mutate(across(.cols = -Class, .fns = ~ {
    breaks <- quantile(., probs = seq(0, 1, by = 1/3), na.rm = TRUE)
    labels <- c("Baixo", "Médio", "Alto")
    print(breaks)
    as.factor(cut(., breaks = breaks, labels = labels, include.lowest = TRUE))
  }))

# letra b
wine_data_discretized$Class <- as.factor(wine_data_discretized$Class)
head(wine_data_discretized)