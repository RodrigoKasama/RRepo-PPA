library("daltoolbox")
wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)
colnames(wine_data)[1] <- "Class"
head(wine_data[c(1:2, 61:62, 131:132),])

# Convertendo os targets de numérico para categórico e utilizando factor !!!!!
wine_data$Class <- factor(wine_data$Class, levels = c(1, 2, 3))


slevels <- levels(wine_data$Class)
# Junta as colunas de features (2 a 13) com a coluna de target -> Orientação
wine_data <- cbind(as.matrix(wine_data[,2:13]), Class=wine_data$Class)

# Fazer encoding por intervalo (https://nbviewer.org/github/cefet-rj-dal/daltoolbox/blob/main/transformation/dal_smoothing_interval.ipynb)
## Encoding das features
### Feature 1: Magnesium - wine_data[6]

smoother <- smoothing_inter(n = 3)
smoother <- fit(smoother, wine_data[,5])
sl_tri <- transform(smoother, wine_data[, 5])
print(table(sl_tri))
smoother$interval
wine_data[, 5] <- factor(wine_data[, 5], levels = (smoother$interval),
                         labels = c("Baixo", "Médio", "Alto"))


### Feature 2: Hue - wine_data[11]
### Feature 3: Proline - wine_data[13]

## Encoding do target (one-hot?)



# Criando os dados de treinamento e teste
set.seed(1)
sr <- sample_random()
# Divide o dataset na relação 80:20
sr <- train_test(sr, wine_data)
wine_train <- sr$train
wine_test <- sr$test

# Exibindo a qntd de cada classe nos dois datasets (Não é desbalanceado) 
distrib_train_test <- rbind(table(wine_data[,"Class"]), 
                            table(wine_train[,"Class"]),
                            table(wine_test[,"Class"]))
rownames(distrib_train_test) <- c("dataset", "training", "test")
head(distrib_train_test)

model <- cla_mlp("Class", slevels, size=3, decay=0.03)
model <- fit(model, wine_train)
train_prediction <- predict(model, wine_train)


wine_train_predictand <- adjust_class_label(wine_train[,"Class"])
train_eval <- evaluate(model, wine_train_predictand, train_prediction)
print(train_eval$metrics)


# Test  
test_prediction <- predict(model, wine_test)

wine_test_predictand <- adjust_class_label(wine_test[,"Class"])
test_eval <- evaluate(model, wine_test_predictand, test_prediction)
print(test_eval$metrics)
