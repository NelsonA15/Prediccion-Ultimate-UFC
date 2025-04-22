#CARGAR DATOS
colnames(ufc_data) <- as.character(ufc_data[1, ])
ufc_data <- ufc_data[-1, ]
colnames(ufc_data)

library(DataExplorer)
create_report(ufc_data)

ufc_pca <- subset(ufc_data, select = c(R_ev, B_ev, B_current_lose_streak, Winner,
                                       B_current_win_streak,
                                       B_draw,
                                       B_avg_SIG_STR_landed,
                                       B_avg_SIG_STR_pct,
                                       B_avg_SUB_ATT,
                                       B_avg_TD_landed,
                                       B_avg_TD_pct,
                                       B_longest_win_streak,
                                       B_losses,
                                       B_total_rounds_fought,
                                       B_total_title_bouts,
                                       B_win_by_Decision_Majority,
                                       B_win_by_Decision_Split,
                                       B_win_by_Decision_Unanimous,
                                       B_win_by_KOTKO,
                                       B_win_by_Submission,
                                       B_win_by_TKO_Doctor_Stoppage,
                                       B_wins,
                                       B_Height_cms,
                                       B_Reach_cms,
                                       B_Weight_lbs,
                                       R_current_lose_streak,
                                       R_current_win_streak,
                                       R_draw,
                                       R_avg_SIG_STR_landed,
                                       R_avg_SIG_STR_pct,
                                       R_avg_SUB_ATT,
                                       R_avg_TD_landed,
                                       R_avg_TD_pct,
                                       R_longest_win_streak,
                                       R_losses,
                                       R_total_rounds_fought,
                                       R_total_title_bouts,
                                       R_win_by_Decision_Majority,
                                       R_win_by_Decision_Split,
                                       R_win_by_Decision_Unanimous,
                                       R_win_by_KOTKO,
                                       R_win_by_Submission,
                                       R_win_by_TKO_Doctor_Stoppage, 
                                       R_wins,
                                       R_Height_cms,
                                       R_Reach_cms,
                                       R_Weight_lbs,
                                       B_age,
                                       R_age,
                                       lose_streak_dif,
                                       win_streak_dif,
                                       longest_win_streak_dif,
                                       win_dif,
                                       loss_dif,
                                       total_round_dif,
                                       total_title_bout_dif,
                                       ko_dif,
                                       sub_dif,
                                       height_dif,
                                       reach_dif,
                                       age_dif,
                                       sig_str_dif,
                                       avg_sub_att_dif,
                                       avg_td_dif
))
library(dplyr)
library(readr)
ufc_pca$Winner <- ifelse(ufc_pca$Winner == "Red", 1, 0)
ufc_pca <- ufc_pca %>%
  mutate(across(everything(), ~na_if(., "")))

ufc_pca <- ufc_pca %>%
  mutate(across(where(is.character), as.numeric))

ufc_pca <- na.omit(ufc_pca)
summary(ufc_pca)


# Entrenamiento (80%) y de validación (20%)
set.seed(123) 
index <- createDataPartition(ufc_pca$Winner, p = 0.8, list = FALSE)
train_data <- ufc_pca[index,]
valid_data <- ufc_pca[-index,]

# PCA
train_data_pca <- train_data[, !names(train_data) %in% c("Winner")]
valid_data_pca <- valid_data[, !names(valid_data) %in% c("Winner")]

pca_train <- prcomp(train_data_pca, center = TRUE, scale. = TRUE)

train_pca <- data.frame(pca_train$x[, 1:25])
valid_pca <- predict(pca_train, newdata = valid_data_pca)[, 1:25]
valid_pca <- as.data.frame(valid_pca)

# Añadir la variable 'Winner'
train_pca$Winner <- train_data$Winner
valid_pca$Winner <- valid_data$Winner


# Ajustar el modelo de regresión logística
modelo_logistico <- glm(Winner ~ ., data = train_pca, family = binomial())
summary(modelo_logistico)

# Predecir la probabilidad de la clase 1 (Winner = 1) en el conjunto de validación
predicciones <- predict(modelo_logistico, newdata = valid_pca, type = "response")

# Convertir las probabilidades a etiquetas de clase basadas en un umbral de 0.5
etiquetas_predichas <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la precisión y la matriz de confusión
library(caret)
conf_mat <- confusionMatrix(factor(etiquetas_predichas), factor(valid_pca$Winner))
conf_mat
