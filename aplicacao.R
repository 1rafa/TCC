source("helper functions.R")


# --- Calcular o preço de uma Renda Mensal temporária postecipada para uma pessoa do sexo masculino dos 65 aos 75 anos
# --- PGBL
ano_inicial <- 2008
idade_inicial <- 65
n <- 10
montante <- 1000000  # Quantidade total paga pelo segurado no começo do contrato (em 2007)
taxa_juros <- 0.01  # taxa de juros mensal fixa utilziada


get_anuidade_mensal_temporaria <- function(df, country, sex, ano_inicial, idade_inicial, n, taxa_juros,
                                           improvment){
  # df deve ter as colunas: Country, Sex, Year, Age, Mx
  tabua <- df %>% subset(Country == country & Sex == sex, select=c(Year, Age, Mx))

  if (improvment){
    temp <- tibble()
    for (i in 0:(n-1)){
        temp <- rbind(temp, subset(tabua, Year == ano_inicial+i & Age == idade_inicial+i)) }
    tabua <- temp } else {
      tabua <- tabua %>% subset(Year == (ano_inicial-1) & Age >= idade_inicial & Age <= (idade_inicial+n-1)) }

  tabua <- tabua %>% mutate(qx=Mx/(1-(Mx/2)))

  vt <- 0
  tpx <- 1
  axn <- 0
  for (t in 1:n){
    vt <- 1/((1+taxa_juros)^t)
    tpx <- tpx*(1-tabua$qx[t])
    axn <- axn + vt*tpx }

  nEx <- vt*tpx
  anuidade_mensal <- (axn + (12-1)/(2*12))*(1-nEx)

  return(anuidade_mensal) }


get_renda_mensal <- function(df, country, sex, ano_inicial, idade_inicial, n, taxa_juros, montante, improvment){
  anuidade_mensal <- get_anuidade_mensal_temporaria(df, country, sex, ano_inicial, idade_inicial, n, taxa_juros,
                                                    improvment)

  fator <- 1/(12*anuidade_mensal)
  renda_mensal <- montante*fator

  return(renda_mensal)}


path <- "./CallBack/lee_carter/df_test.csv"
df_teste <- read_csv(path)

#df <- df_teste %>% select(Country, Sex, Year, Age, Mx_forecast) %>% rename(Mx=Mx_forecast)
df <- df_teste %>% select(Country, Sex, Year, Age, Mx)

df_renda <- tibble()
for (country in populacoes){
  for (sex in sexos){
    renda_local <- get_renda_mensal(df, country, sex, ano_inicial, idade_inicial, n, taxa_juros, montante, improvment = FALSE)
    df_renda_local <- tibble(Country=country, Sex=sex, Renda=renda_local)
    df_renda <- rbind(df_renda, df_renda_local)
  }
}

path <- "./CallBack/df_renda_sem_improvment.csv"
write_csv(df_renda, path)
