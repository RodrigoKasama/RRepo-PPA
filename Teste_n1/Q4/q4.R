library("daltoolbox")
wine_dataset_path = "Teste_n1/vinhos.csv"
wine_data <- read.csv(wine_dataset_path, header = TRUE)
wine_data

head(wine_data[c(1:2, 61:62, 131:132),])

# Convertendo os targets de numérico para categórico e utilizando factor !!!!!
wine_data$Type <- factor(wine_data$Type, levels = c(1, 2, 3), labels = c("Baixo", "Médio", "Alto"))


slevels <- levels(wine_data$Type)

wine_data <- cbind(as.matrix(wine_data[,1:4]), Type=wine_data$Type)

# Criando os dados de treinamento e teste
set.seed(1)
sr <- sample_random()
# Divide o dataset em 80:20
sr <- train_test(sr, wine_data)
wine_train <- sr$train
wine_test <- sr$test

# Exibindo a qntd de cada classe nos dois datasets (Não é desbalanceado) 
tbl <- rbind(table(wine_data[,"Type"]), 
             table(wine_train[,"Type"]), 
             table(wine_test[,"Type"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

model <- cla_mlp("Type", slevels, size=3, decay=0.03)
model <- fit(model, wine_train)
train_prediction <- predict(model, wine_train)


wine_train_predictand <- adjust_class_label(wine_train[,"Type"])
train_eval <- evaluate(model, wine_train_predictand, train_prediction)
print(train_eval$metrics)


# Test  
test_prediction <- predict(model, wine_test)

wine_test_predictand <- adjust_class_label(wine_test[,"Type"])
test_eval <- evaluate(model, wine_test_predictand, test_prediction)
print(test_eval$metrics)
