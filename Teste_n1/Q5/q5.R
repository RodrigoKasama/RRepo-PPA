wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)

colnames(wine_data)[1] <- "Class"
head(wine_data[c(1:2, 61:62, 131:132),])

#Criando regras de associação
rules <- apriori(wine_data, parameter = list(supp = 0.3, conf = 0.8, target = "rules"))
inspect(rules)

