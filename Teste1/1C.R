library(ggplot2)
library(dplyr)

vinhos <- read.csv("vinhos.csv", header = FALSE, sep = ",")

colnames(vinhos)[1] <- "Tipo"

vinhos$Tipo <- factor(vinhos$Tipo, levels = sort(unique(vinhos$Tipo)))

for (i in 2:ncol(vinhos)) {
  p <- ggplot(vinhos, aes(x = vinhos[[i]], fill = Tipo)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribuição de Densidade para", colnames(vinhos)[i]),
         x = colnames(vinhos)[i],
         y = "Densidade") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  ggsave(filename = paste0("graficos_1C/densidade_", colnames(vinhos)[i], ".png"), plot = p)
}
