#CARGAR DATOS
colnames(ufc) <- as.character(ufc[1, ])
ufc <- ufc[-1, ]
colnames(ufc_data)


# DISTANCIA DE GOWER
colnames(ufc)
library(cluster)
ufc[1:10,]

#FACTORES
library(cluster)

# Corrección  numéricas
numeric_vars <- c("B_current_lose_streak", "B_current_win_streak", "B_avg_SIG_STR_landed",
                  "B_avg_SIG_STR_pct", "B_avg_SUB_ATT", "B_avg_TD_landed", "B_avg_TD_pct",
                  "B_longest_win_streak", "B_losses", "B_total_rounds_fought", "B_total_title_bouts",
                  "B_win_by_Decision_Majority", "B_win_by_Decision_Split", "B_win_by_Decision_Unanimous",
                  "B_win_by_KO/TKO", "B_win_by_Submission", "B_win_by_TKO_Doctor_Stoppage", "B_wins",
                  "R_current_lose_streak", "R_current_win_streak", "R_avg_SIG_STR_landed", "R_avg_SIG_STR_pct",
                  "R_avg_SUB_ATT", "R_avg_TD_landed", "R_avg_TD_pct", "R_longest_win_streak", "R_losses",
                  "R_total_rounds_fought", "R_total_title_bouts", "R_win_by_Decision_Majority", "R_win_by_Decision_Split",
                  "R_win_by_Decision_Unanimous", "R_win_by_KO/TKO", "R_win_by_Submission", "R_win_by_TKO_Doctor_Stoppage",
                  "R_wins", "R_age", "B_age")

ufc[numeric_vars] <- lapply(ufc[numeric_vars], function(x) as.numeric(as.character(x)))

# Verificar categóricas
categorical_vars <- c("R_fighter", "B_fighter", "Winner", "title_bout", "weight_class", "gender",
                      "B_Stance", "R_Stance", "better_rank", "empty_arena")
# Assuming your correct data frame is ufc_data
ufc[categorical_vars] <- lapply(ufc[categorical_vars], as.factor)

# Check if all necessary columns are present
print(colnames(ufc_data))

summary(ufc)
str(ufc)

ufc_f <-  subset(ufc, select = c(weight_class, gender, R_ev, B_ev, B_current_lose_streak,
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
                                 avg_td_dif))



# Corregir tipos de datos
numeric_columns <- c("R_ev", "B_ev", "B_Height_cms", "B_Reach_cms", "B_Weight_lbs", "R_Height_cms", "R_Reach_cms", "R_Weight_lbs",
                     "lose_streak_dif", "win_streak_dif", "longest_win_streak_dif", "win_dif", "loss_dif", 
                     "total_round_dif", "total_title_bout_dif", "ko_dif", "sub_dif", "height_dif", "reach_dif", 
                     "age_dif", "sig_str_dif", "avg_sub_att_dif", "avg_td_dif")

ufc_f[numeric_columns] <- lapply(ufc_f[numeric_columns], function(x) as.numeric(as.character(x)))

# Asegurarse de que no hay NA introducidos por errores en la conversión
sum(is.na(ufc_f))

factor_columns <- c( "B_draw", "R_draw")
ufc_f[factor_columns] <- lapply(ufc_f[factor_columns], as.factor)

str(ufc_f)
ufc_f_p <-  subset(ufc_f, select = c(weight_class, gender, location ,R_ev, B_ev, B_current_lose_streak,
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
                                     R_win_by_Submission,
                                     R_win_by_TKO_Doctor_Stoppage, 
                                     R_wins,
                                     R_Height_cms,
                                     R_Reach_cms,
                                     R_Weight_lbs,
                                     B_age,
                                     R_age))


# Ahora calcular la distancia de Gower usando 'daisy' de la biblioteca cluster
library(cluster)
gower_dist <- daisy(ufc_f, metric = "gower")

# Visualizar la estructura de la matriz de distancias
str(gower_dist)

# Escalamiento multidimensional (MDS)
mds <- cmdscale(gower_dist, k = 30, eig = TRUE, add = FALSE, x.ret = FALSE)
mds
# Crear un data frame con los resultados
mds_df <- data.frame(Dim1 = mds$points[, 1], Dim2 = mds$points[, 2])

# Visualización con ggplot2
ggplot(mds_df, aes(x = Dim1, y = Dim2)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "MDS usando Distancia de Gower",
       x = "Dimensión 1", y = "Dimensión 2") +
  theme(plot.title = element_text(hjust = 0.5))

# Aplicar K-means clustering
set.seed(123)

kmeans_result <- kmeans(mds_df, centers = 3, nstart = 25)


library(plotly)
# 3 dimensiones
mds_3d <- cmdscale(gower_dist, k = 3, eig = TRUE, add = FALSE, x.ret = TRUE)

mds_df_3d <- data.frame(Dim1 = mds_3d$points[, 1], Dim2 = mds_3d$points[, 2], Dim3 = mds_3d$points[, 3])

# Visualización 3D
plot_ly(data = mds_df_3d, x = ~Dim1, y = ~Dim2, z = ~Dim3, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = mds_df_3d$Dim3, colorscale = 'Viridis')) %>%
  layout(title = "3D Visualization of MDS using Gower Distance",
         scene = list(xaxis = list(title = 'Dimensión 1'),
                      yaxis = list(title = 'Dimensión 2'),
                      zaxis = list(title = 'Dimensión 3')))


