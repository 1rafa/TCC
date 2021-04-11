source("helper functions.R")

username <- ""  # escreva o username do HMD (geralmente um email)
password <- "" # escreva a senha do HMD

df_raw <- read_mx_web_all(username, password)
path <- "./Dados/df_raw.csv"
write_csv(df_raw, path)

df_all <- df_raw %>%
          subset(Country %in% populacoes
                 & Age <= lim_superior_idade
                 & Year >= lim_inferior_ano
                 & Year <= lim_superior_ano+anos_futuros)
df_all <- df_all %>% remove_na_mx() %>% mutate(LogMx=log(Mx)) %>% relocate(ImputedFlag, .after = last_col())

#df_all %>% subset(ImputedFlag==1) %>% group_by(Country) %>% summarise(n_imputed = n()) %>% view()
#df_all %>% subset(is.na(Mx)) %>% view()

path <- "./Dados/df_all.csv"
write_csv(df_all, path)

exposure_raw <- read_exposure_web_all(username, password)
path <- "./Dados/exposure_raw.csv"
write_csv(exposure_raw, path)

exposure_all <- exposure_raw %>%
                subset(Country %in% populacoes
                       & Age <= lim_superior_idade
                       & Year >= lim_inferior_ano
                       & Year <= lim_superior_ano+anos_futuros)
exposure_all <- exposure_all %>% remove_na_exposure()

path <- "./Dados/exposure_all.csv"
write_csv(exposure_all, path)

#exposure_all %>% subset(ImputedFlag==1) %>% group_by(Year) %>% summarise(n_imputed = n()) %>% view()
#exposure_all %>% subset(ImputedFlag==1) %>% group_by(Country) %>% summarise(n_imputed = n()) %>% view()
#exposure_all %>% subset(ImputedFlag==1) %>% group_by(Age) %>% summarise(n_imputed = n()) %>% view()
#exposure_all %>% subset(is.na(Exposure)) %>% view()

