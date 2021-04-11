source("helper functions.R")
#######################################################################################################################
# --- Análise de Loss e Validation Loss --- #
#######################################################################################################################
# - LSTM
temp2 <- tibble()
for (country in populacoes){
  for (sex in sexos){
    temp1 <- subset(metrics_all, Country == country & Sex == sex)
    temp1 <- temp1 %>% mutate(x=seq_along(temp1$Country))
    temp2 <- rbind(temp1, temp2) } }
metrics_all <- temp2

ggplot(subset(metrics_all, x > 20 & Sex == "Male")) +
  geom_line(aes(x=x, y=loss), color="red") +
  geom_line(aes(x=x, y=val_loss), color="blue") +
  facet_wrap(vars(Country), ncol=4) +
  labs(title="Loss por epoch (Homem)", x="epoch", y="Loss")

ggplot(subset(metrics_all, x > 20 & Sex == "Female")) +
  geom_line(aes(x=x, y=loss), color="red") +
  geom_line(aes(x=x, y=val_loss), color="blue") +
  facet_wrap(vars(Country), ncol=4) +
  labs(title="Loss por epoch (Mulher)", x="epoch", y="Loss")

# - MLP e LSTM-MLP
temp <- tibble(loss = metrics_all$loss, val_loss = metrics_all$val_loss, x = seq_along(metrics_all$val_loss))
ggplot(subset(temp, x > 20)) +
  geom_line(aes(x=x, y=loss), color="red") +
  geom_line(aes(x=x, y=val_loss), color="blue")

#######################################################################################################################
# --- Análise da previsão - Erro --- #
#######################################################################################################################
# --- Ler o arquivo
path <- "./CallBack/lee_carter/df_test.csv"
df_test <- read_csv(path)
df_test <- df_test %>% subset(ImputedFlag == 0) %>% mutate(ImputedFlag=NULL)
df_test <- df_test %>% mutate(error=(df_test$Mx-df_test$Mx_forecast))

# --- EQM por país
df_mse_in <- df_test %>%
             subset(Year <= lim_superior_ano) %>%
             group_by(Country, Sex) %>%
             summarise(mse = round(mean(error^2, na.rm=TRUE)*10^4, digits=2)) %>%
             pivot_wider(names_from="Sex", values_from="mse")
df_mse_out <- df_test %>%
              subset(Year > lim_superior_ano) %>%
              group_by(Country, Sex) %>%
              summarise(mse = round(mean(error^2, na.rm=TRUE)*10^4, digits=2)) %>%
              pivot_wider(names_from="Sex", values_from="mse")
df_mse <- tibble(Country=df_mse_in$Country, Male_in=df_mse_in$Male, Female_in=df_mse_in$Female,
                 Male_out = df_mse_out$Male, Female_out=df_mse_out$Female)

path <- "./CallBack/lstm_mlp/df_mse.csv"
write_csv(df_mse, path)

# --- EQM Total
# Erro quadrático médio In sample
mse_in_male <- df_test %>%
               subset(Year <= lim_superior_ano & Sex == "Male") %>%
               .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4
mse_in_male <- round(mse_in_male, digits=2)
mse_in_female <- df_test %>%
               subset(Year <= lim_superior_ano & Sex == "Female") %>%
               .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4 %>% round(digits=2)
mse_in_female <- round(mse_in_female, digits=2)

# Erro quadrático médio Out of sample
mse_out_male <- df_test %>%
               subset(Year > lim_superior_ano & Sex == "Male") %>%
               .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4
mse_out_male <- round(mse_out_male, digits=2)

mse_out_female <- df_test %>%
               subset(Year > lim_superior_ano & Sex == "Female") %>%
               .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4
mse_out_female <- round(mse_out_female, digits=2)

print(paste0("mse_in_male: ", mse_in_male,
             ", mse_in_female: ", mse_in_female,
             ", mse_out_male: ", mse_out_male,
             ", mse_out_female: ", mse_out_female))

# --- EQP por intervalos de 5 anos:
paths <- c("./CallBack/lee_carter/df_test.csv",
           "./CallBack/lstm/df_test.csv",
           "./CallBack/mlp/df_test.csv",
           "./CallBack/lstm_mlp/df_test.csv")
mse_by_age <- tibble()
for (path in paths){
  df_test <- read_csv(path)
  df_test <- df_test %>% subset(ImputedFlag == 0) %>% mutate(ImputedFlag=NULL)
  df_test <- df_test %>% mutate(error=(df_test$Mx-df_test$Mx_forecast))
  lower_age_local <- 0
  for (i in 1:(lim_superior_idade/5)){
    upper_age_local <- i*5
    mse_out_male <- df_test %>%
                   subset(Year > lim_superior_ano
                          & Sex == "Male"
                          & Age >= lower_age_local
                          & Age < upper_age_local) %>%
                   .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4
    mse_out_male <- round(mse_out_male, digits=8)
    mse_out_female <- df_test %>%
                   subset(Year > lim_superior_ano
                          & Sex == "Female"
                          & Age >= lower_age_local
                          & Age < upper_age_local) %>%
                   .$error %>% .^2 %>% mean(na.rm=TRUE)*10^4
    mse_out_female <- round(mse_out_female, digits=8)
    mse_by_age <- rbind(mse_by_age, tibble(Path = path,
                                           LowerAge=lower_age_local, UpperAge=upper_age_local,
                                           Male=mse_out_male, Female=mse_out_female))
    lower_age_local <- upper_age_local } }

mse_by_age <- mse_by_age %>% mutate(LogMSE=log(MSE))

view(mse_by_age)

ggplot(mse_by_age) +
  geom_line(aes(x=LowerAge, y=MSE, color=Path)) +
  facet_wrap(vars(Sex))

#######################################################################################################################
# --- Gráficos --- #
#######################################################################################################################
path <- "./CallBack/lee_carter/df_test.csv"
df_lc <- read_csv(path)
df_lc <- df_lc %>% mutate(modelo="Lee Carter")
path <- "./CallBack/lstm/df_test.csv"
df_lstm <- read_csv(path)
df_lstm <- df_lstm %>% mutate(modelo="LSTM")
path <- "./CallBack/mlp/df_test.csv"
df_mlp <- read_csv(path)
df_mlp <- df_mlp %>% mutate(modelo="MLP")
path <- "./CallBack/lstm_mlp/df_test.csv"
df_lstm_mlp <- read_csv(path)
df_lstm_mlp <- df_lstm_mlp %>% mutate(modelo="LSTM-MLP")
df_insample <- df_lstm_mlp %>% mutate(LogMx_forecast=LogMx, modelo="InSample")

df_test <- rbind(df_lc, df_lstm, df_mlp, df_lstm_mlp, df_insample)

ggplot(subset(df_test, Year == lim_superior_ano+anos_futuros & Sex == "Male")) +
  geom_line(aes(x=Age, y=LogMx_forecast, color=modelo)) +
  scale_color_manual(values=c("black", "blue", "green", "red", "purple")) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Mx por país em 2017 (Homens)", x="Ano", y="Mx", color="")

ggplot(subset(df_test, Year == lim_superior_ano+anos_futuros & Sex == "Female")) +
  geom_line(aes(x=Age, y=LogMx_forecast, color=modelo)) +
  scale_color_manual(values=c("black", "blue", "green", "red", "purple")) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Mx por país em 2017 (Mulheres)", x="Ano", y="Mx", color="")





# --- Gráficos
path <- "./CallBack/lee_carter/df_test.csv"
df_lc <- read_csv(path)
df_lc <- df_lc %>% mutate(modelo="Lee Carter", error=(Mx-Mx_forecast))
path <- "./CallBack/lstm/df_test.csv"
df_lstm <- read_csv(path)
df_lstm <- df_lstm %>% mutate(modelo="LSTM", error=(Mx-Mx_forecast))
path <- "./CallBack/mlp/df_test.csv"
df_mlp <- read_csv(path)
df_mlp <- df_mlp %>% mutate(modelo="MLP", error=(Mx-Mx_forecast))
path <- "./CallBack/lstm_mlp/df_test.csv"
df_lstm_mlp <- read_csv(path)
df_lstm_mlp <- df_lstm_mlp %>% mutate(modelo="LSTM-MLP", error=(Mx-Mx_forecast))
df_test <- rbind(df_lc, df_lstm, df_mlp, df_lstm_mlp)

ggplot(subset(df_test, Year == lim_superior_ano+anos_futuros & Sex == "Male")) +
  geom_line(aes(x=Age, y=error, color=modelo)) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Erro por país em 2017 (Homens)", x="Idade", y="Erro absoluto em log(Mx)", color="")

ggplot(subset(df_test, Year == lim_superior_ano+anos_futuros & Sex == "Female")) +
  geom_line(aes(x=Age, y=error, color=modelo)) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Erro por país em 2017 (Mulheres)", x="Idade", y="Erro absoluto em log(Mx)", color="")
