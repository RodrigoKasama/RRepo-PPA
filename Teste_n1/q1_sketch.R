
wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)
wine_data

head(wine_data[c(1:2, 61:62, 131:132),])

get_stats <- function(campo){
  c(
    resumo = summary(campo),
    DesvioPadrao = sd(campo, na.rm = TRUE)
       )
}

analise_per_campo <- lapply(wine_data, get_stats)
analise_per_campo


# Rascunho
# "daltoolbox")
grf <- plot_hist(wine_data %>% dplyr::select(Hue), label_x = "Hue")
plot(grf)

#colors <- brewer.pal(4, 'Set1')
#font <- theme(text = element_text(size=16))
