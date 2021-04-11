# --- LEE-CARTER ---
source("helper functions.R")


lstm <- function(df, country, sex, model, file_name){
  df_all <- df %>% subset(Country == country & Sex == sex)

  df_train <- df_all %>%
              subset(Age <= lim_superior_idade
                     & Year >= lim_inferior_ano
                     & Year <= lim_superior_ano)
  data_train <- prep_lstm(df_train, timestamps, pad)
  stored_vars_mx <- data_train$stored_vars_mx

  df_val <- df_all %>%
            subset(Age <= lim_superior_idade
                   & Year > lim_superior_ano-anos_futuros
                   & Year <= lim_superior_ano+anos_futuros)
  data_val <- prep_lstm(df_val, timestamps, pad, stored_vars_mx = stored_vars_mx)

  # define callback
  CBs <- callback_reduce_lr_on_plateau(patience=20)

  # training model
  fit <- model %>% fit(x=data_train$x, y=data_train$y, batch_size=256, validation_data=list(data_val$x, data_val$y),
                       epochs=300, callbacks=CBs, workers=4, use_multiprocessing=TRUE)
  # Save model
  save_model_hdf5(model, file_name)

  # --- Predict
  y_in_sample <- predict(model, data_train$x)
  y_in_sample <- y_in_sample %>%
                min_max_scaler_inv(stored_vars_mx) %>%
                rbind(matrix(nrow=anos_futuros*(lim_superior_idade+1))) %>%
                as.vector()

  df_test <- df_all %>% subset(Age <= lim_superior_idade &
                               Year >= lim_inferior_ano+timestamps &
                               Year <= lim_superior_ano+anos_futuros) %>%
                        mutate(LogMx_forecast=y_in_sample)

  for (i in 1:anos_futuros){
    df_local <- df_test %>% subset(Year >= lim_superior_ano-timestamps+i
                                   & Year <= lim_superior_ano+i-1)
    data_local <- prep_lstm(df_local, timestamps, pad, stored_vars_mx, return_y = FALSE)

    y_forecast <- predict(model, data_local$x) %>%
                        min_max_scaler_inv(stored_vars_mx) %>%
                        as.vector()

    idx <- ((total_anos-timestamps+i-1)*(lim_superior_idade+1)+1):((total_anos-timestamps+i)*(lim_superior_idade+1))
    df_test$LogMx_forecast[idx] <- y_forecast }

  df_test <- df_test %>% mutate(Mx_forecast=exp(LogMx_forecast))

  fit_metrics <- tibble(loss = as.vector(fit$metrics$loss),
                        val_loss = as.vector(fit$metrics$val_loss),
                        Country = country,
                        Sex=sex)

  stored_vars_mx <- tibble(Min = stored_vars_mx[[1]],
                           Max = stored_vars_mx[[2]],
                           Country = country,
                           Sex=sex)

  return(list(model=model, metrics=fit_metrics, stored_vars_mx=stored_vars_mx, df=df_test)) }


# Read data
path <- "./Dados/df_all.csv"
df_all <- read_csv(path, col_types = list(col_character(),
                                          col_character(),
                                          col_integer(),
                                          col_integer(),
                                          col_double(),
                                          col_double(),
                                          col_integer() ) )

models <- list()
for (country in populacoes){
  for (sex in sexos){
    name_model <- paste0(Sys.Date(), "_", country, "_", sex)
    file_name <- paste0("./CallBack/lstm/", name_model)

    model <- model_lstm(timestamps, pad)
    models[[name_model]] <- lstm(df_all, country, sex, model, file_name) } }

df_test <- tibble()
metrics_all <- tibble()
stored_vars_all <- tibble()
for (model in models){
  df_test <- rbind(df_test, model$df)
  metrics_all <- rbind(metrics_all, model$metrics)
  stored_vars_all <- rbind(stored_vars_all, model$stored_vars_mx) }

df_test <- df_test %>% arrange(Country, Sex, Year, Age)
metrics_all <- metrics_all %>% arrange(Country, Sex)
stored_vars_all <- stored_vars_all %>% arrange(Country, Sex)

path <- "./CallBack/lstm/df_test.csv"
write_csv(df_test, path)
path <- "./CallBack/lstm/metrics_all.csv"
write_csv(metrics_all, path)
path <- "./CallBack/lstm/stored_vars.csv"
write_csv(stored_vars_all, path)
