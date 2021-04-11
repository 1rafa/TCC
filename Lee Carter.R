# --- LEE-CARTER ---
source("helper functions.R")


lee_carter <- function(df_all, exposure_all, country, sex){
  df_train <- df_all %>% subset(Country == country
                                & Sex == sex
                                & Age <= lim_superior_idade
                                & Year >= lim_inferior_ano
                                & Year <= lim_superior_ano)

  # Reshape
  log_mx <- df_train %>%
            subset(select=c(Year, Age, LogMx)) %>%
            pivot_wider(id_cols=Year, names_from=Age, values_from=LogMx) %>%
            mutate(Year=NULL) %>%
            data.matrix()

  # Calcula o parâmetro Ax
  ax_long <- apply(log_mx, 2, mean)
  ax_wide <- t(matrix(rep(ax_long, total_anos), (lim_superior_idade + 1), total_anos))

  # Executa a decomposição SVD
  svd <- svd(log_mx-ax_wide)

  # Estimação de Kx e Bx
  bx <- svd$v[, 1] / sum(svd$v[, 1]) 	# estimativa bx
  kt <- svd$d[1] * svd$u[, 1] * sum(svd$v[, 1]) 	# estimativa kt

  record_kt <- list(kt = kt)

  # --- Ajuste do Kt por person-years lived
  # Dados da exposição
  exposure <- exposure_all %>% subset(Country == country
                                            & Sex == sex
                                            & Age <= lim_superior_idade
                                            & Year >= lim_inferior_ano
                                            & Year <= lim_superior_ano)
  # Reshape
  exposure <- exposure %>%
              subset(select=c(Year, Age, Exposure)) %>%
              pivot_wider(id_cols=Year, names_from=Age, values_from=Exposure) %>%
              mutate(Year=NULL) %>%
              data.matrix()

  mx <- df_train %>%
        subset(select=c(Year, Age, Mx)) %>%
        pivot_wider(id_cols=Year, names_from=Age, values_from=Mx) %>%
        mutate(Year=NULL) %>%
        data.matrix()

  new_kt <- matrix(NA, total_anos, 1)
  for (i in 1:total_anos){
    funkt <- function(kt_local){
      sum(abs(exposure[i,] * (mx[i,] - exp(ax_long + kt_local * bx)))) }
    new_kt[i] <- optim(kt[i], funkt)$par
  }
  kt <- new_kt

  record_kt$adjusted_kt <- kt

  # Previsão para períodos futuros
  ts_kt <- kt %>%
           data.frame(kt=kt) %>%
           mutate(Year=lim_inferior_ano:lim_superior_ano) %>%
           as_tsibble(index=Year)

  arima <- ts_kt %>% model(arima=ARIMA(kt, greedy = FALSE))

  forecast_kt <- forecast(arima, h=anos_futuros)

  kt <- rbind(kt, matrix(forecast_kt$.mean))

  # ---
  ax_widest <- t(matrix(rep(ax_long, total_anos+anos_futuros), (lim_superior_idade + 1), total_anos+anos_futuros))
  y_forecast <- exp(ax_widest + kt %*% t(bx))

  df_forecast <- y_forecast %>%
                 data.frame() %>%
                 setNames(0:100) %>%
                 mutate(Year=lim_inferior_ano:(lim_superior_ano+anos_futuros)) %>%
                 pivot_longer(cols=!Year, names_to="Age", values_to="Mx")

  df_test <- df_all %>% subset(Country == country
                               & Sex == sex
                               & Age <= lim_superior_idade
                               & Year >= lim_inferior_ano
                               & Year <= lim_superior_ano+anos_futuros)

  df_test <- df_test %>% mutate(Mx_forecast=df_forecast$Mx, LogMx_forecast=log(Mx_forecast))

  params <- list(ax=ax_long, kt=kt, bx=bx)

  return(list(df=df_test, params=params, record_kt=record_kt, arima_kt = arima))}


# Read data
path <- "./Dados/df_all.csv"
df_all <- read_csv(path, col_types = list(col_factor(),
                                          col_factor(),
                                          col_integer(),
                                          col_integer(),
                                          col_double(),
                                          col_double(),
                                          col_integer()))
path <- "./Dados/exposure_all.csv"
exposure_all <- read_csv(path)

models <- list()
for (country in populacoes){
  for (sex in sexos){
    name_model <- paste0(Sys.Date(), "_", country, "_", sex)
    models[[name_model]] <- lee_carter(df_all, exposure_all, country, sex) } }

df_test <- tibble()
df_ax <- tibble()
df_bx <- tibble()
df_kt <- tibble()
for (model in models){
  df_test <- rbind(df_test, model$df)

  df_ax_local <- tibble(Country = model$df$Country[1],
                        Sex = model$df$Sex[1],
                        Age=0:(length(model$params$ax)-1),
                        Ax=model$params$ax)

  df_bx_local <- tibble(Country = model$df$Country[1],
                        Sex = model$df$Sex[1],
                        Age=0:(length(model$params$bx)-1),
                        Bx=model$params$bx)

  df_kt_local <- tibble(Country = model$df$Country[1],
                        Sex = model$df$Sex[1],
                        Year=min(model$df$Year):max(model$df$Year),
                        Kt=model$params$kt)

  df_ax <- rbind(df_ax, df_ax_local)
  df_bx <- rbind(df_bx, df_bx_local)
  df_kt <- rbind(df_kt, df_kt_local)
  }

df_test <- df_test %>% arrange(Country, Sex, Year, Age)

path <- "./CallBack/lee_carter/df_test.csv"
write_csv(df_test, path)
path <- "./CallBack/lee_carter/df_ax.csv"
write_csv(df_ax, path)
path <- "./CallBack/lee_carter/df_bx.csv"
write_csv(df_bx, path)
path <- "./CallBack/lee_carter/df_kt.csv"
write_csv(df_kt, path)

path <- "./CallBack/lee_carter/df_test.csv"
df_test <- read_csv(path)
path <- "./CallBack/lee_carter/df_ax.csv"
df_ax <- read_csv(path)
path <- "./CallBack/lee_carter/df_bx.csv"
df_bx <- read_csv(path)
path <- "./CallBack/lee_carter/df_kt.csv"
df_kt <- read_csv(path)


# --- Viualização dos parâmetros estimados:
ggplot(df_ax) +
  geom_line(aes(x=Age, y=Ax, color=Sex)) +
  scale_color_manual(values=c("deeppink", "blue")) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Ax por país", x="Idade", y="Ax", color="Sexo biológico")

ggplot(df_bx) +
  geom_line(aes(x=Age, y=Bx, color=Sex)) +
  scale_color_manual(values=c("deeppink", "blue")) +
  facet_wrap(vars(Country), scales = "free", ncol=5) +
  labs(title="Bx por país", x="Idade", y="Bx", color="Sexo biológico")

df_kt <- df_kt %>% mutate(Forecast_flag = ifelse(Year>lim_superior_ano, "Forecast", "Insample"))
ggplot(subset(df_kt)) +
  geom_line(aes(x=Year, y=Kt, color=Sex, linetype=Forecast_flag)) +
  scale_color_manual(values=c("deeppink", "blue")) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  facet_wrap(vars(Country), ncol=5) +
  labs(title="Kt por país", x="Ano", y="Kt", color="", linetype="")