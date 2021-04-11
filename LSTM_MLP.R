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
                   & Year <= lim_superior_ano)

data_train <- prep_lstm_mlp(df_train, timestamps, pad)

n_count <- data_train$n_count
stored_vars <- data_train$stored_vars

df_val <- df_all %>%
          subset(Age <= lim_superior_idade
                 & Year > lim_superior_ano-anos_futuros
                 & Year <= lim_superior_ano+anos_futuros)
data_val <- prep_lstm_mlp(df_val, timestamps, pad, stored_vars = stored_vars)

# LSTM_MLP
model <- model_lstm_mlp(data_train$n_count$country,
                        data_train$n_count$sex,
                        data_train$n_count$age,
                        timestamps, pad)

# define callback
CBs <- callback_reduce_lr_on_plateau(patience=20)

# training model
proc_time <- proc.time()
fit <- model %>% fit(x=list(data_train$x$x, data_train$x$country, data_train$x$sex,
                            data_train$x$year, data_train$x$age),
                     y=data_train$y,
                     batch_size=256,
                     validation_data=list(list(data_val$x$x, data_val$x$country, data_val$x$sex,
                                               data_val$x$year, data_val$x$age),
                                          data_val$y),
                     epochs=200,
                     callbacks=CBs,
                     workers=4,
                     use_multiprocessing=TRUE)
print(proc.time() - proc_time)

# Save model
name_model <- paste0(Sys.Date(), "_lstm_mlp")
file_name <- paste0("./CallBack/lstm_mlp/", name_model)
save_model_hdf5(model, file_name)
#model <- load_model_hdf5(file_name)

metrics_all <- tibble(loss = as.vector(fit$metrics$loss),
                      val_loss = as.vector(fit$metrics$val_loss))
path <- "./CallBack/lstm_mlp/metrics_all.csv"
write_csv(metrics_all, path)

#model <- load_model_hdf5(file_name)

# --- Predict
df_test <- NULL
for (country in populacoes){
  for (sex in sexos){
    df_local <- df_all %>%
                subset(Age <= lim_superior_idade
                       & Year >= lim_inferior_ano
                       & Year <= lim_superior_ano
                       & Country == country
                       & Sex == sex)
    data_local <- prep_lstm_mlp(df_local, timestamps, pad, stored_vars = stored_vars)
    
    y_in_sample <- predict(model, list(data_local$x$x, data_local$x$country, data_local$x$sex,
                                  data_local$x$year, data_local$x$age))
    y_in_sample <- y_in_sample %>%
                   min_max_scaler_inv(stored_vars$mx) %>%
                   rbind(matrix(nrow=anos_futuros*(lim_superior_idade+1))) %>%
                   as.vector()

    df_local <- df_all %>% subset(Age <= lim_superior_idade
                                  & Year >= lim_inferior_ano+timestamps
                                  & Year <= lim_superior_ano+anos_futuros
                                  & Country == country
                                  & Sex == sex) %>%
                           mutate(LogMx_forecast=y_in_sample) %>% arrange(Year, Age)
    
    for (i in 1:anos_futuros){
      df_temp <- df_local %>% subset(Year >= lim_superior_ano-timestamps+i &
                                     Year <= lim_superior_ano+i-1)
      data_temp <- prep_lstm_mlp(df_temp, timestamps, pad, return_y = FALSE, stored_vars = stored_vars)
      
      y_temp <- predict(model, list(data_temp$x$x, data_temp$x$country, data_temp$x$sex,
                                    data_temp$x$year, data_temp$x$age)) %>%
                  min_max_scaler_inv(stored_vars$mx) %>%
                  as.vector()
      
      idx <- ((total_anos-timestamps+i-1)*(lim_superior_idade+1)+1):((total_anos-timestamps+i)*(lim_superior_idade+1))
      df_local$LogMx_forecast[idx] <- y_temp }

    df_test <- bind_rows(df_test, df_local) } }

df_test <- df_test %>% mutate(Mx_forecast=exp(LogMx_forecast))

path <- "./CallBack/lstm_mlp/df_test.csv"
write_csv(df_test, path)


# ---
weights_country <- get_weights(model)[[4]]
weights_sex <- get_weights(model)[[5]]
weights_age <- get_weights(model)[[6]]

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
