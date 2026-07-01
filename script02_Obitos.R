library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)


# CARREGAR BANCO DE DADOS ORIGINAL ----------------------------------------

df <- read.csv2("Banco_original/03_df_paraquat.csv")
df <- df |> filter(EVOLUCAO=="Obito por intoxicacao exogena")

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



# -------------------------------------------------------------------------
###########################################################################
# FREQUENCIAS TEMPO -------------------------------------------------------
# Ano
freqAno <- df %>% group_by(NU_ANO) %>% 
  summarise(N = n())

# Mes
freqMes <- df %>% group_by(MES_IS) %>% 
  summarise(N = n())

# Matriz Ano/Mes
freqAnoMes <- df %>% group_by(NU_ANO, MES_IS) %>% 
  summarise(N = n())


# -------------------------------------------------------------------------
###########################################################################
# GRAFICOS TEMPO ----------------------------------------------------------
# Ano
freqAno %>% 
  ggplot(aes(x = factor(NU_ANO), y = N))+
  geom_col(fill = 'darkblue')+
  geom_text(aes(label = N), 
            vjust = -0.5,       # Posiciona o texto um pouco acima da barra
            size = 3.5,         # Tamanho da fonte do rótulo
            color = "black") + 
  labs(x=NULL,
       y="Registros",
       fill='')+
    theme_bw()

# Mes
freqMes %>% 
  ggplot(aes(x = MES_IS, y = N))+
  geom_col(fill = 'darkblue')+
  labs(x=NULL,
       y="Registros")+
  theme_bw()


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



# -------------------------------------------------------------------------
###########################################################################
# EXPOSIÇÃO ---------------------------------------------------------------

# Local da exposição
local_exposicao <- tabyl(dat = df,
                      var1 = LOC_EXPO,
                      var2 = CS_SEXO, 
                      show_na = TRUE, 
                      show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); local_exposicao

# Grupo do agente toxico
agente_tox <- tabyl(dat = df,
                         var1 = AGENTE_TOX,
                         var2 = CS_SEXO, 
                         show_na = TRUE, 
                         show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); agente_tox

# Via de exposicao (se Agrotoxico)
## Via 1
via_expo1 <- tabyl(dat = df[df$AGENTE_TOX %in% c('Agrotoxico agricola',
                                                'Agrotoxico domestico',
                                                'Agrotoxico spublica'),],
                         var1 = VIA_1,
                         var2 = CS_SEXO, 
                         show_na = TRUE, 
                         show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); via_expo1

## Via 2
via_expo2 <- tabyl(dat = df[df$AGENTE_TOX %in% c('Agrotoxico agricola',
                                                 'Agrotoxico domestico',
                                                 'Agrotoxico spublica'),],
                   var1 = VIA_2,
                   var2 = CS_SEXO, 
                   show_na = TRUE, 
                   show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); via_expo2

## Via 3
via_expo3 <- tabyl(dat = df[df$AGENTE_TOX %in% c('Agrotoxico agricola',
                                                 'Agrotoxico domestico',
                                                 'Agrotoxico spublica'),],
                   var1 = VIA_3,
                   var2 = CS_SEXO, 
                   show_na = TRUE, 
                   show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); via_expo3


# Circunstancia de exposicao
circunstancia <- tabyl(dat = df,
                       var1 = CIRCUNSTAN,
                       var2 = CS_SEXO, 
                       show_na = TRUE, 
                       show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); circunstancia


# Exposicao no trabalho
expo_trabalho <- tabyl(dat = df,
                       var1 = DOENCA_TRA,
                       var2 = CS_SEXO, 
                       show_na = TRUE, 
                       show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); expo_trabalho

# Tipo exposicao
tipo_expo <- tabyl(dat = df,
                   var1 = TPEXP,
                   var2 = CS_SEXO, 
                   show_na = TRUE, 
                   show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); tipo_expo



# -------------------------------------------------------------------------
###########################################################################
# ATENDIMENTO -------------------------------------------------------------

# Tempo Decorrido entre a Exposição e o Atendimento (horas, dias, ...)
# Tipo de atendimento (hospitalar, ambulatorial ...)
# Houve hospitalização (sim, não)



# -------------------------------------------------------------------------
###########################################################################
# CLASSIFICACAO E EVENTOS ASSOCIADOS --------------------------------------

# Classificacao
classificacao <- tabyl(dat = df,
                   var1 = CLASSI_FIN,
                   var2 = CS_SEXO, 
                   show_na = TRUE, 
                   show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); classificacao

# Criterio confirmacao
criterio <- tabyl(dat = df,
                     var1 = CRITERIO,
                     var2 = CS_SEXO, 
                     show_na = TRUE, 
                     show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); criterio

# Evolucao
evolucao <- tabyl(dat = df,
                  var1 = EVOLUCAO,
                  var2 = CS_SEXO, 
                  show_na = TRUE, 
                  show_missing_levels = TRUE) %>% 
  adorn_totals(where = "col"); evolucao

