source("helper functions.R")

# Read data
path <- "./Dados/df_all.csv"
df_all <- read_csv(path, col_types = list(col_character(),
                                          col_character(),
                                          col_integer(),
                                          col_integer(),
                                          col_double(),
                                          col_double(),
                                          col_integer()))

df_train <- df_all %>%
            subset(Age <= lim_superior_idade
                   & Year >= lim_inferior_ano
                   & Year <= lim_superior_ano
                   & ImputedFlag == 0)
data_train <- prep_mlp(df_train)

n_count <- data_train$n_count
stored_vars <- data_train$stored_vars

df_val <- df_all %>%
          subset(Age <= lim_superior_idade
                 & Year > lim_superior_ano
                 & Year <= lim_superior_ano+anos_futuros
                 & ImputedFlag == 0)
data_val <- prep_mlp(df_val, stored_vars)

# MLP
model <- model_mlp(n_count$country, n_count$sex, n_count$age)

# define callback
CBs <- callback_reduce_lr_on_plateau(patience=20)

# training model
proc_time <- proc.time()
fit <- model %>% fit(x=list(data_train$x$country, data_train$x$sex, data_train$x$year, data_train$x$age),
                     y=data_train$y,
                     batch_size=512,
                     validation_data=list(list(data_val$x$country, data_val$x$sex, data_val$x$year, data_val$x$age),
                                          data_val$y),
                     epochs=150,
                     callbacks=CBs,
                     workers=4,
                     use_multiprocessing=TRUE)
print(proc.time() - proc_time)

# Save model
name_model <- paste0(Sys.Date(), "_mlp")
file_name <- paste0("./CallBack/mlp/", name_model)
save_model_hdf5(model, file_name)
#model <- load_model_hdf5(file_name)

metrics_all <- tibble(loss = as.vector(fit$metrics$loss),
                      val_loss = as.vector(fit$metrics$val_loss))
path <- "./CallBack/mlp/metrics_all.csv"
write_csv(metrics_all, path)

# --- Predict
df_test <- df_all %>%
           subset(Age <= lim_superior_idade
                  & Year >= lim_inferior_ano
                  & Year <= lim_superior_ano+anos_futuros
                  & ImputedFlag == 0)
data_test <- prep_mlp(df_test, stored_vars)

y_forecast <- predict(model, list(data_test$x$country, data_test$x$sex, data_test$x$year, data_test$x$age))
y_forecast <- y_forecast %>%
              min_max_scaler_inv(stored_vars$mx) %>%
              exp()

df_test <- df_test %>% mutate(Mx_forecast=as.vector(y_forecast))
df_test <- df_test %>% mutate(LogMx_forecast=log(Mx_forecast))

path <- "./CallBack/mlp/df_test.csv"
write_csv(df_test, path)

# ---

weights_country <- get_weights(model)[[1]]
weights_sex <- get_weights(model)[[2]]
weights_age <- get_weights(model)[[3]]

pca <- princomp(weights_age)
pca_scores <- tibble(comp1=pca$scores[,1],
                     comp2=pca$scores[,2],
                     comp3=pca$scores[,3],
                     comp4=pca$scores[,4],
                     comp5=pca$scores[,5],
                     Ages=0:100)
ggplot(pca_scores, aes(x=comp1, y=comp2)) +
  geom_point() +
  geom_text(aes(label=Ages),hjust=0, vjust=0) +
  labs(title="Duas primeiras dimensões do PCA por Idade")

pca <- princomp(weights_country)
pca_scores <- tibble(comp1=pca$scores[,1],
                     comp2=pca$scores[,2],
                     comp3=pca$scores[,3],
                     comp4=pca$scores[,4],
                     comp5=pca$scores[,5],
                     Countries=populacoes)
ggplot(pca_scores, aes(x=comp1, y=comp2)) +
  geom_point() +
  geom_text(aes(label=Countries),hjust=0, vjust=0) +
  labs(title="Duas primeiras dimensões do PCA por País")
