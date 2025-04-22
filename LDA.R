#CARGAR DATOS
colnames(ufc_data) <- as.character(ufc_data[1, ])
ufc_data <- ufc_data[-1, ]
colnames(ufc_data)
str(ufc_data)
library(DataExplorer)
create_report(ufc_data)

ufc_pca <- subset(ufc_data, select = c(R_ev, B_ev, B_current_lose_streak,
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
                                       B_win_by_Decision_Unanimous,                                       B_win_by_Submission,
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

ufc_pca$Winner <- ufc_data$Winner
ufc_pca$Winner <- ifelse(ufc_pca$Winner == "Red", 1, 0)

ufc_pca <- na.omit(ufc_pca)
summary(ufc_pca)


# Entrenamiento (80%) y de validación (20%)
set.seed(123) 
index <- createDataPartition(ufc_pca$Winner, p = 0.8, list = FALSE)
train_data <- ufc_pca[index,]
valid_data <- ufc_pca[-index,]


# Calcular la matriz de correlación de las variables predictoras
cor_matrix <- cor(train_data[, sapply(train_data, is.numeric)])
print(cor_matrix)

# Identificar pares de variables con alta correlación
high_cor <- findCorrelation(cor_matrix, cutoff = 0.5, verbose = TRUE)
print(high_cor)

# Eliminar variables altamente correlacionadas basado en el índice proporcionado por 'findCorrelation'
train_data_reduced <- train_data[, -high_cor]
valid_data_reduced <- valid_data[, -high_cor]

# Reintentar el modelo LDA con datos reducidos
lda_model_reduced <- lda(Winner ~ ., data = train_data_reduced)

summary(lda_model_reduced)

# Predecir y evaluar con datos reducidos
predictions_reduced <- predict(lda_model_reduced, valid_data_reduced)
accuracy_reduced <- mean(predictions_reduced$class == valid_data_reduced$Winner)
print(paste("Accuracy:", accuracy_reduced))

predictions_reduced$class <- factor(predictions_reduced$class, levels = c("0", "1"))
valid_data_reduced$Winner <- factor(valid_data_reduced$Winner, levels = c("0", "1"))

library(caret)
conf_mat <- confusionMatrix(predictions_reduced$class, valid_data_reduced$Winner)
print(conf_mat)


  
