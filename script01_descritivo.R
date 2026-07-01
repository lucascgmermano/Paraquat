library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)


# CARREGAR BANCO DE DADOS ORIGINAL ----------------------------------------

df <- read_excel("Banco_original/03_df_paraquat.xlsx")


# -------------------------------------------------------------------------
###########################################################################
# AJUSTES -----------------------------------------------------------------

# Data Inicio Sintomas
df$DT_SIN_PRI <- df$DT_SIN_PRI %>% as_date()

# Obter mês
df$MES_IS <- month(df$DT_SIN_PRI, label = TRUE, abbr = TRUE)

# Obter ano
df$ANO_IS <- year(df$DT_SIN_PRI)

# Recodificar estados
df['SG_UF'] <- df$SG_UF %>% as.integer()

df$SG_UF <- case_match(df$SG_UF, 11 ~ "RO",12 ~ "AC",13 ~ "AM",14 ~ "RR",
                       15 ~ "PA",16 ~ "AP",17 ~ "TO",21 ~ "MA",22 ~ "PI",
                       23 ~ "CE",24 ~ "RN",25 ~ "PB",26 ~ "PE",27 ~ "AL",
                       28 ~ "SE",29 ~ "BA",31 ~ "MG",32 ~ "ES",33 ~ "RJ",
                       35 ~ "SP",41 ~ "PR",42 ~ "SC",43 ~ "RS",50 ~ "MS",
                       51 ~ "MT",52 ~ "GO",53 ~ "DF")

# Criar variavel REGIAO
df$REGIAO <- case_match(
  df$SG_UF,
  c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
  c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
  c("DF", "GO", "MS", "MT") ~ "Centro-Oeste",
  c("ES", "MG", "RJ", "SP") ~ "Sudeste",
  c("PR", "RS", "SC") ~ "Sul",
  .default = NA_character_ # Define como NA se houver alguma sigla inválida
)



# Sexo indefinido
df[df['CS_SEXO']=="I",'CS_SEXO'] <- "M"

# COLUNAS -----------------------------------------------------------------

colnames(df)

# -------------------------------------------------------------------------
###########################################################################
# FREQUENCIAS TEMPO -------------------------------------------------------
## Casos
# Ano
freqAno <- df %>% group_by(ANO_IS) %>% 
  summarise(N = n())

freqMes <- df %>% group_by(MES_IS) %>% 
  summarise(N = n())

freqAnoMes <- df %>% group_by(ANO_IS, MES_IS) %>% 
  summarise(N = n())

## Obitos

obitosAno <- df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% group_by(ANO_IS) %>% 
  summarise(N = n())

obitosMes <- df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% group_by(MES_IS) %>% 
  summarise(N = n())

obitosAnoMes <- df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% group_by(ANO_IS, MES_IS) %>% 
  summarise(N = n())



# faltantes <- data.frame(ANO_IS = c(2024,2024,2022,2007,2007,2007),
#            MES_IS = c('mai','jun','jul','mai','out','dez'),
#            N = c(0,0,0,0,0,0)
# )

# freqAnoMes <- rbind(freqAnoMes,faltantes)


# -------------------------------------------------------------------------
###########################################################################
# GRAFICOS TEMPO ----------------------------------------------------------

## Casos
freqAno %>% 
  ggplot(aes(x = factor(ANO_IS), y = N))+
  geom_col(fill = 'darkblue')+
  geom_text(aes(label = N), 
            vjust = -0.5,       # Posiciona o texto um pouco acima da barra
            size = 3.5,         # Tamanho da fonte do rótulo
            color = "black") + 
  labs(x=NULL,
       y="Registros",
       fill='')+
    theme_bw()

freqMes %>% 
  ggplot(aes(x = MES_IS, y = N))+
  geom_col(fill = 'darkblue')+
  labs(x=NULL,
       y="Registros")+
  theme_bw()

freqAnoMes %>% 
  ggplot(aes(x = MES_IS, y = factor(ANO_IS), fill = N)) +
  geom_tile(color = "gray", lwd = 0.5) +
  labs(x=NULL,
       y="Ano")+
  scale_fill_gradient(low = "white", high = "firebrick3") +
  theme(rect = element_rect(fill = NULL, color = NULL))

## Obitos
obitosAno %>% 
  ggplot(aes(x = factor(ANO_IS), y = N))+
  geom_col(fill = 'firebrick3')+
  geom_text(aes(label = N), 
            vjust = -0.5,       # Posiciona o texto um pouco acima da barra
            size = 3.5,         # Tamanho da fonte do rótulo
            color = "black") + 
  labs(x=NULL,
       y="Registros",
       fill='')+
  theme_bw()

obitosMes %>% 
  ggplot(aes(x = MES_IS, y = N))+
  geom_col(fill = 'firebrick3')+
  labs(x=NULL,
       y="Registros")+
  theme_bw()

obitosAnoMes %>% 
  ggplot(aes(x = MES_IS, y = factor(ANO_IS), fill = N)) +
  geom_tile(color = "gray", lwd = 0.5) +
  labs(x=NULL,
       y="Ano")+
  scale_fill_gradient(low = "white", high = "firebrick3") +
  theme(rect = element_rect(fill = NULL, color = NULL))




# -------------------------------------------------------------------------
###########################################################################
# REGIAO, ESTADO E MUNICIPIO DA EXPOSICAO --------------------------------------
## Casos
# Regiao
df %>% 
  group_by(REGIAO) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 28)

# Estado
df %>% 
  group_by(SG_UF) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 28)

# Município
df %>% 
  group_by(SG_UF, NOME_MUNI) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 20)


## Obitos
# Regiao
df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% 
  group_by(REGIAO) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 28)

# Estado
df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% 
  group_by(SG_UF) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 28)

# Município
df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% 
  group_by(SG_UF, NOME_MUNI) %>% 
  summarise(N = n()) %>% 
  #na.omit() %>% 
  arrange(desc(N)) %>% print(n = 20)



# -------------------------------------------------------------------------
###########################################################################
# SOCIODEMOGRAFICOS -------------------------------------------------------

# Idade (continuo)
summary(df$IDADE)

# Faixa etaria
faixaEtaria <- tabyl(dat = df,
                     var1 = FAIXA_ETARIA,
                     var2 = CS_SEXO) %>% 
  adorn_totals(where = "col"); faixaEtaria

# Raça
raca <- tabyl(dat = df,
              var1 = CS_RACA,
              var2 = CS_SEXO) %>% 
  adorn_totals(where = "col"); raca

# Gestação
gestacao <- tabyl(dat = df,
              var1 = CS_GESTANT,
              var2 = CS_SEXO) %>% 
  adorn_totals(where = "col"); gestacao

# Escolaridade
escolaridade <- tabyl(dat = df,
                  var1 = CS_ESCOL_N,
                  var2 = CS_SEXO) %>% 
  adorn_totals(where = "col"); escolaridade



df[df$EVOLUCAO=="Obito por intoxicacao exogena",] %>% View()
df[df$EVOLUCAO=="Obito por intoxicacao exogena","ID_MN_RESI"] %>% is.na() %>% sum()

write.csv2(x = df[df$EVOLUCAO=="Obito por intoxicacao exogena",],
           file = "ducida.csv")
