library(dplyr)
library(tidyr)

colnames(ufc_data) <- as.character(ufc_data[1, ])
ufc_data <- ufc_data[-1, ]
colnames(ufc_data)

summary(ufc_data)

numeric_vars <- c("B_avg_SIG_STR_landed", "B_avg_SIG_STR_pct", "B_avg_SUB_ATT",
                  "B_avg_TD_landed", "B_avg_TD_pct", "B_Height_cms", "B_Reach_cms", "B_Weight_lbs",
                  "R_avg_SIG_STR_landed", "R_avg_SIG_STR_pct", "R_avg_SUB_ATT",
                  "R_avg_TD_landed", "R_avg_TD_pct", "R_Height_cms", "R_Reach_cms", "R_Weight_lbs",
                  "B_current_lose_streak", "B_current_win_streak", "R_current_lose_streak",
                  "R_current_win_streak", "B_longest_win_streak", "R_longest_win_streak",
                  "B_losses", "R_losses", "B_total_rounds_fought", "R_total_rounds_fought")

ufc_data[numeric_vars] <- lapply(ufc_data[numeric_vars], function(x) as.numeric(as.character(x)))
ufc_data <- drop_na(ufc_data)
selected_vars <- c(numeric_vars, "Winner")  
final_data <- ufc_data[selected_vars]
summary(final_data)

library(caret)
library(FactoMineR)

# Asegúrate de eliminar la columna 'Winner' para el PCA, ya que es categórica
final_data_numeric <- final_data[, -which(names(final_data) == "Winner")]

# Aplicar PCA
pca_results <- prcomp(final_data_numeric, scale. = TRUE)

# Ver resultados del PCA
summary(pca_results)

# Calcula la varianza explicada por cada componente principal
var_exp <- pca_results$sdev^2 / sum(pca_results$sdev^2)  # Calcula la varianza explicada de cada componente
cum_var_exp <- cumsum(var_exp)  # Calcula la varianza explicada acumulada

# Imprime los valores para verificar que se calculen correctamente
print(var_exp)
print(cum_var_exp)

# Crear un gráfico de scree plot para la varianza explicada
plot(cum_var_exp, xlab = "Número de Componentes Principales", ylab = "Varianza Explicada Acumulada",
     type = 'b', pch = 19, col = "blue", main = "Scree Plot de Varianza Explicada")
abline(h = 0.95, col = "red", lty = 2)  # Línea de corte para el 95% de la varianza explicada

# Biplot de los dos primeros componentes principales
biplot(pca_results, scale = 0)  # 'scale = 0' para evitar el reescalado de las flechas


# Crear un nuevo dataframe con las primeras 18 componentes principales y la variable objetivo
svm_data <- data.frame(pca_results$x[, 1:18], Winner = final_data$Winner)
svm_data$Winner <- ifelse(svm_data$Winner == "Red", 1, 0)


# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_indices <- createDataPartition(svm_data$Winner, p = 0.8, list = TRUE)$Resample1
train_data <- svm_data[train_indices, ]
test_data <- svm_data[-train_indices, ]


library(e1071)

# Entrenar el modelo SVM con kernel radial
weights <- c('0' = 1, '1' = 2)

# Entrenar el modelo SVM con pesos de clase específicos
svm_model <- svm(Winner ~ ., data = train_data, kernel = "radial", cost = 5, scale = FALSE, class.weights = weights)


# Revisar el modelo
summary(svm_model)

# Hacer predicciones en el conjunto de prueba
predictions <- predict(svm_model, test_data[-which(names(test_data) == "Winner")])

# Calcular y mostrar la matriz de confusión
confusion_matrix <- table(Predictions = predictions, Actual = test_data$Winner)
print(confusion_matrix)

# Calcular la precisión
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)

table(final_data$Winner)

