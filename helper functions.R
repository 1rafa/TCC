library(HMDHFDplus)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(tsibble)
library(fable)
library(caret)
library(keras)


# Variáveis Globais
lim_inferior_idade <- 0
lim_superior_idade <- 100
lim_inferior_ano <- 1968
lim_superior_ano <- 2007
total_anos <- lim_superior_ano - lim_inferior_ano + 1

anos_futuros <- 10
timestamps <- 10
pad <- 2

populacoes <- c("AUS", "AUT", "BEL", "BGR", "BLR", "CHE", "CZE", "DEUTE", "DEUTW", "DNK", "ESP", "EST", "FIN",
                "FRACNP", "FRATNP", "GBR_NIR", "GBR_NP", "GBR_SCO", "GBRCENW", "GBRTENW", "HUN", "IRL", "ITA", "JPN",
                "LTU", "LVA", "NLD", "NOR", "POL", "PRT", "SVK", "SWE", "USA")
sexos <- c("Male", "Female")


ler_exposicao_web <- function(populacao, username, password){
  df <- readHMDweb(populacao, "Exposures_1x1", username, password, fixup=TRUE)
  df <- df[c("Year", "Age", "Female", "Male")]
  df <- melt(df, id.var=c('Year','Age'), variable.name='Sex')
  df <- df %>% arrange(Sex, Year, Age) %>% rename(Exposure=value)

  return(df) }


read_exposure_web_all <- function(username, password){
  countries <- getHMDcountries()

  df <- read_exposure_web(countries[1], username, password)
  df$Country <- countries[1]
  for (country in countries[2:length(countries)]){
    temp <- read_exposure_web(country)
    temp$Country <- country
    df <- rbind(df, temp) }
  df <- arrange(df, Country, Sex, Year, Age)
  
  return(df) }


read_mx_web <- function(country, username, password){
  # Importando Mortalidades
  df <- readHMDweb(country, "Mx_1x1", username, password, fixup=TRUE)
  df <- df[c("Year", "Age", "Female", "Male")]
  df <- melt(df, id.var=c('Year','Age'), variable.name='Sex')
  df <- df %>% arrange(df, Sex, Year, Age) %>% rename(Mx=value)

  return(df) }


read_mx_web_all <- function(username, password){
  countries <- getHMDcountries()

  df <- read_mx_web(countries[1], username, password)
  df$Country <- countries[1]
  for (country in countries[2:length(countries)]){
    temp <- read_mx_web(country)
    temp$Country <- country
    df <- rbind(df, temp) }
  df <- arrange(df, Country, Sex, Year, Age)
  
  return(df) }


remove_na_mx <- function(df){
  temp <- df %>% group_by(Sex, Year, Age) %>% summarise(Mx = mean(Mx, na.rm=TRUE))
  df <- df %>% complete(Country, Sex, Year, Age)
  df$ImputedFlag <- 0
  for (i in seq_along(df$Mx)){
      if ( is.na(df$Mx[i]) || (df$Mx[i] == 0) ){
        idx_local <- (temp$Sex == df$Sex[i] & temp$Year == df$Year[i] & temp$Age == df$Age[i])
        if ( isTRUE(any(idx_local)) ){
          df$Mx[i] <- subset(temp, idx_local)$Mx
          df$ImputedFlag[i] <- 1 }
        else{
          df$Mx[i] <- NA } } }
  return(df) }


remove_na_exposure <- function(df){
  temp <- df %>% group_by(Sex, Year, Age) %>% summarise(Exposure = mean(Exposure, na.rm=TRUE))
  df <- df %>% complete(Country, Sex, Year, Age)
  df$ImputedFlag <- 0
  for (i in seq_along(df$Exposure)){
      if ( is.na(df$Exposure[i]) || (df$Exposure[i] == 0) ){
        idx_local <- (temp$Sex == df$Sex[i] & temp$Year == df$Year[i] & temp$Age == df$Age[i])
        if ( isTRUE(any(idx_local)) ){
          df$Exposure[i] <- subset(temp, idx_local)$Exposure
          df$ImputedFlag[i] <- 1 }
        else{
          df$Exposure[i] <- NA } } }
  return(df) }


prep_helper <- function(df, timestamps, pad, return_y=TRUE){
  lowerboundyear <- min(df$Year)

  scaled_mx <- df %>%
        subset(select=c(Year, Age, ScaledMx)) %>%
        pivot_wider(id_cols=Year, names_from=Age, values_from=ScaledMx) %>%
        mutate(Year=NULL) %>%
        data.matrix()

  if (return_y){
    n_years <- nrow(scaled_mx) - timestamps }
  else{
    n_years <- nrow(scaled_mx) - timestamps + 1 }

  n_ages <- ncol(scaled_mx)
  n_train <- n_years * n_ages # number of training samples
  
  # adding x_lengthding at the border
  obs_length <- pad * 2 + 1

  if (pad > 0){
    for (i in 1:pad){
      scaled_mx <- as.matrix(cbind(scaled_mx[, 1], scaled_mx, scaled_mx[, ncol(scaled_mx)])) } }

  data_train <- list(x = array(dim=c(n_train, timestamps, obs_length)),
                     y = matrix(nrow=n_train),
                     year = matrix(nrow=n_train),
                     age = matrix(nrow=n_train),
                     imputed_flag = matrix(nrow=n_train) )

  index <- 0
  for (year in (1:n_years)){
    for (age in (1:n_ages)){
      index <- index+1
      data_train$x[index,,] <- scaled_mx[year:(timestamps + year - 1), age:(age + pad*2)]
      if (return_y){
        data_train$y[index] <- scaled_mx[timestamps+year, age+pad]
        data_train$imputed_flag[index] <- df %>% subset(Year == (lowerboundyear + timestamps + year - 1)
                                                        & Age == (age-1)) %>% .$ImputedFlag }
      else{
        data_train$imputed_flag[index] <- 0 }
      data_train$year[index] <- lowerboundyear + timestamps + year - 1
      data_train$age[index] <- age - 1 } }

    return(data_train) }


prep_helper_conj <- function(df, timestamps, pad, return_y=TRUE){
  countries <- unique(df$Country)
  sexes <- unique(df$Sex)
  years <- unique(df$Year)
  ages <- unique(df$Age)

  if (return_y){
    n_samples <- length(countries) * length(sexes) * (length(years) - timestamps) * (length(ages)) }
  else{
    n_samples <- length(countries) * length(sexes) * (length(years) - timestamps + 1) * (length(ages)) }

  data_samples <- list(country = matrix(nrow=n_samples),
                       sex = matrix(nrow=n_samples),
                       year = matrix(nrow=n_samples),
                       age = matrix(nrow=n_samples),
                       imputed_flag = matrix(nrow=n_samples),
                       x = array(NA, dim=c(n_samples, timestamps, (pad*2+1))),
                       y = matrix(nrow=n_samples) )

  temp <- 1
  for (country in countries){
    for (sex in sexes){
      df_local <- df %>% subset(Country == country & Sex == sex)
      data_local <- prep_helper(df_local, timestamps, pad, return_y)

      n_local <- nrow(data_local$year)
      
      idx <- temp:(temp+n_local-1)
      
      data_samples$country[idx] <- country
      data_samples$sex[idx] <- sex
      data_samples$year[idx] <- data_local$year
      data_samples$age[idx] <- data_local$age
      data_samples$imputed_flag[idx] <- data_local$imputed_flag
      data_samples$x[idx, , ] <- data_local$x
      data_samples$y[idx] <- data_local$y

      temp <- temp + n_local } }

  return(data_samples) }


prep_lstm <- function(df, timestamps, pad, stored_vars_mx=NA, return_y=TRUE){

  temp <- min_max_scaler(df$LogMx, stored_vars_mx)
  df$ScaledMx <- as.vector(temp$x)
  stored_vars_mx <- temp$stored_vars

  data_train <- prep_helper(df, timestamps, pad, return_y)

  return(list(x = data_train$x, y = data_train$y, stored_vars_mx = stored_vars_mx)) }


prep_mlp <- function(df, stored_vars=NA){
  if (all(is.na(stored_vars))){
    stored_vars <- list(mx=NA, sex=NA, country=NA, year=NA, age=NA) }
  n_count <- list(country=NA, sex=NA, age=NA)
  x_train <- list(country=NA, sex=NA, year=NA, age=NA)

  # Mx
  temp <- min_max_scaler(df$LogMx, stored_vars$mx)
  y_train <- as.vector(temp$x)
  stored_vars$mx <- temp$stored_vars
  # Country
  temp <- label_to_integer(df$Country, stored_vars$country)
  x_train$country <- temp$x
  stored_vars$country <- temp$stored_vars
  n_count$country <- length(stored_vars$country)
  # Sex
  temp <- label_to_integer(df$Sex, stored_vars$sex)
  x_train$sex <- temp$x
  stored_vars$sex <- temp$stored_vars
  n_count$sex <- length(stored_vars$sex)
  # Year
  temp <- min_max_scaler(df$Year, stored_vars$year)
  x_train$year <- temp$x
  stored_vars$year <- temp$stored_vars
  # Age
  x_train$age <- df$Age
  n_count$age <- length(unique(x_train$age))

  return(list(x=x_train, y=y_train, stored_vars=stored_vars, n_count=n_count))}


prep_lstm_mlp <- function(df, timestamps, pad, return_y=TRUE, stored_vars=NA){

  if (all(is.na(stored_vars))){
    stored_vars <- list(mx=NA, sex=NA, country=NA, year=NA, age=NA) }
  n_count <- list(country=NA, sex=NA, age=NA)
  
  temp <- min_max_scaler(df$LogMx, stored_vars$mx)
  df$ScaledMx <- as.vector(temp$x)
  stored_vars$mx <- temp$stored_vars
  
  temp <- prep_helper_conj(df, timestamps, pad, return_y)
  x_train <- temp[c("x", "country", "sex", "year", "age", "imputed_flag")]
  y_train <- temp$y

  # Country
  temp <- label_to_integer(x_train$country, stored_vars$country)
  x_train$country <- temp$x
  stored_vars$country <- temp$stored_vars
  n_count$country <- length(stored_vars$country)
  # Sex
  temp <- label_to_integer(x_train$sex, stored_vars$sex)
  x_train$sex <- temp$x
  stored_vars$sex <- temp$stored_vars
  n_count$sex <- length(stored_vars$sex)
  # Year
  temp <- min_max_scaler(x_train$year, stored_vars$year)
  x_train$year <- temp$x
  stored_vars$year <- temp$stored_vars
  # Age
  n_count$age <- length(unique(x_train$age))

  return(list(x=x_train, y=y_train, stored_vars=stored_vars, n_count=n_count))}


min_max_scaler <- function(x, stored_vars = NA){
  if (any(is.na(stored_vars))){
    min <- min(x)
    max <- max(x)
    stored_vars <- list(min, max) }
  else{
    min <- stored_vars[[1]]
    max <- stored_vars[[2]] }

  x <- as.matrix((2 * ((x - min) / (max-min)) - 1)) # scalling between -1 and 1

  return(list(x=x, stored_vars=stored_vars)) }


min_max_scaler_inv <- function(x, stored_vars){
  min <- stored_vars[[1]]
  max <- stored_vars[[2]]
  x <- (((x + 1) / 2) * (max-min)) + min
  return(x) }


label_to_integer <-function(x, stored_vars = NA){
  x <- as.vector(x)
  if (all(is.na(stored_vars))){
    stored_vars <- sort(unique(x))
    temp <- seq_along(stored_vars)
    names(temp) <- stored_vars
    stored_vars <- temp
  }
  x <- as.matrix(recode(x, !!!stored_vars))
  return(list(x=x, stored_vars=stored_vars)) }


label_to_integer_inv <-function(x, stored_vars){
  x <- as.vector(x)

  temp <- names(stored_vars)
  names(temp) <- stored_vars

  x <- as.matrix(recode(x, !!!temp))
  return(x) }


model_lstm <- function(timestamps, pad){
  input <- layer_input(shape=c(timestamps, (pad*2+1)), dtype='float32', name='Input')
  middle <- input %>% layer_cudnn_lstm(units=20,
                                       return_sequences = TRUE,
                                       name='LSTM1') %>%
                      layer_cudnn_lstm(units=15,
                                       return_sequences = TRUE,
                                       name='LSTM2') %>%
                      layer_cudnn_lstm(units=10,
                                       name='LSTM3')

  output <- middle %>% layer_dense(units=1, activation="tanh", name="Output")

  model <- keras_model(inputs=input, outputs=output)
  model %>% compile(loss='mean_squared_error', optimizer="adam")
  return(model) }


model_mlp <- function(n_country, n_sex, n_age){
  country_input <- layer_input(shape=1, dtype='int32', name='Country')
  sex_input <- layer_input(shape=1, dtype='int32', name='Sex')
  year_input <- layer_input(shape=1, dtype='float32', name='Year')
  age_input <- layer_input(shape=1, dtype='int32', name='Age')

  country_embed <- country_input %>%
                  layer_embedding(input_dim=n_country, output_dim=5, input_length=1, name='CountryEmbed') %>%
                  layer_flatten()
  sex_embed <- sex_input %>%
              layer_embedding(input_dim=n_sex, output_dim=5, input_length=1, name='SexEmbed') %>%
              layer_flatten()
  age_embed <- age_input %>%
              layer_embedding(input_dim=n_age, output_dim=5, input_length=1, name='AgeEmbed') %>%
              layer_flatten()

  features <- layer_concatenate(list(country_embed, sex_embed, year_input, age_embed), name="Features")

  middle <- features %>%
            layer_dense(units=128, activation="tanh") %>%
            layer_batch_normalization() %>%
            layer_dropout(0.05) %>%
            layer_dense(units=128, activation="tanh") %>%
            layer_batch_normalization() %>%
            layer_dropout(0.05) %>%
            layer_dense(units=128, activation="tanh") %>%
            layer_batch_normalization() %>%
            layer_dropout(0.05) %>%
            layer_dense(units=128, activation="tanh") %>%
            layer_batch_normalization() %>%
            layer_dropout(0.05)

  output <- layer_concatenate(list(features, middle)) %>%
            layer_dense(units=128,
                        activation="tanh") %>%
            layer_batch_normalization() %>%
            layer_dropout(0.05) %>%
            layer_dense(units=1, activation='tanh', name='Output')

  model <- keras_model(inputs=list(country_input, sex_input, year_input, age_input), outputs=output)
  model %>% compile(loss='mean_squared_error', optimizer="adam")

  return(model) }


model_lstm_mlp <- function(n_country, n_sex, n_age, timestamps, pad){
  mx_input <- layer_input(shape=c(timestamps, (pad*2+1)), dtype='float32', name='Mx')
  country_input <- layer_input(shape=1, dtype='int32', name='Country')
  sex_input <- layer_input(shape=1, dtype='int32', name='Sex')
  year_input <- layer_input(shape=1, dtype='float32', name='Year')
  age_input <- layer_input(shape=1, dtype='int32', name='Age')

  country_embed <- country_input %>%
                   layer_embedding(input_dim=n_country,
                                   output_dim=5,
                                   input_length=1,
                                   name='CountryEmbed') %>%
                   layer_flatten()
  sex_embed <- sex_input %>%
               layer_embedding(input_dim=n_sex,
                               output_dim=5,
                               input_length=1,
                               name='SexEmbed') %>%
               layer_flatten()
  age_embed <- age_input %>%
              layer_embedding(input_dim=n_age,
                              output_dim=5,
                              input_length=1,
                              name='AgeEmbed') %>%
              layer_flatten()

  rnn <- mx_input %>% layer_cudnn_lstm(units=20,
                                       return_sequences = TRUE,
                                       name='LSTM1') %>%
                      layer_cudnn_lstm(units=15,
                                       return_sequences = TRUE,
                                       name='LSTM2') %>%
                      layer_cudnn_lstm(units=10,
                                       name='LSTM3')

  features <- layer_concatenate(list(country_embed, sex_embed, age_embed, year_input, rnn), name="Features")

  output <- features %>%
            layer_dense(units=128, activation="tanh") %>%
            layer_dense(units=1, activation='tanh', name='Output')

  model <- keras_model(inputs=list(mx_input, country_input, sex_input, year_input, age_input), outputs=output)
  model %>% compile(loss='mean_squared_error', optimizer="adam")

  return(model) }
