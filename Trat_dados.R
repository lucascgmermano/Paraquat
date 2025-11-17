library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(readr)

# Leitura dos dados
df <- readRDS("df_todos.rds")
cbo <- read.csv2(file = "CBO-Ocupacao.csv",
                 encoding = "latin1",
                 sep = "\t") %>% 
  mutate(CODIGO = as.character(CODIGO))


# 1. Definição dos Limites (Breaks)
# 0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80 e 120 (como limite superior)
limites_sinan <- c(0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf)

# 2. Definição dos Rótulos (Labels)
rotulos_sinan <- c(
  "Menor de 1 ano", 
  "1 a 4 anos", 
  "5 a 9 anos", 
  "10 a 14 anos", 
  "15 a 19 anos", 
  "20 a 29 anos", 
  "30 a 39 anos", 
  "40 a 49 anos", 
  "50 a 59 anos", 
  "60 a 69 anos", 
  "70 a 79 anos", 
  "80 anos ou mais"
)



df %>% mutate(across(.cols = c("NU_ANO","ANO_NASC"), .fns = as.factor),
              SEM_NOT = as.factor(substr(x=SEM_NOT, start = 5, stop = 6)),
              SEM_PRI = as.factor(substr(x=SEM_PRI, start = 5, stop = 6)),
              IDADE = as.numeric(substr(x=NU_IDADE_N, start = 2, stop = 4)),
              IDADE = case_when(IDADE < 1 ~ 0,
                                IDADE >= 1 ~ IDADE),
              CS_GESTANT = case_when(CS_GESTANT == "1" ~ "1 semestre",
                                     CS_GESTANT == "2" ~ "2 semestre",
                                     CS_GESTANT == "3" ~ "3 semestre",
                                     CS_GESTANT == "4" ~ "Idade gestacional ign",
                                     CS_GESTANT == "5" ~ "Nao",
                                     CS_GESTANT == "6" ~ "Nao se aplica ",
                                     CS_GESTANT == "9" ~ NA_character_),
              CS_RACA = case_when(CS_RACA == "1" ~ "Branca",
                                  CS_RACA == "2" ~ "Preta",
                                  CS_RACA == "3" ~ "Amarela",
                                  CS_RACA == "4" ~ "Parda",
                                  CS_RACA == "5" ~ "Indigena",
                                  CS_RACA == "9" ~ NA_character_),
              CS_ESCOL_N = case_when(CS_ESCOL_N == "00" ~ "Analfabeto",
                                     CS_ESCOL_N == "01" ~ "1 a 4 incompleto",
                                     CS_ESCOL_N == "02" ~ "4a completa",
                                     CS_ESCOL_N == "03" ~ "5 a 8 incompleto",
                                     CS_ESCOL_N == "04" ~ "Fundamental completo",
                                     CS_ESCOL_N == "05" ~ "Medio incompleto",
                                     CS_ESCOL_N == "06" ~ "Medio completo",
                                     CS_ESCOL_N == "07" ~ "Superior incompleto",
                                     CS_ESCOL_N == "08" ~ "Superior completo",
                                     CS_ESCOL_N == "10" ~ "Nao se aplica",
                                     CS_ESCOL_N == "09" ~ NA_character_),
              FAIXA_ETARIA = cut(x = IDADE,                  # Ja retorna valores em factor
                                 breaks = limites_sinan,
                                 labels = rotulos_sinan,
                                 include.lowest = TRUE,      # Inclui o limite inferior (0 ano)
                                 right = FALSE),              # Define o intervalo como [início, fim), ou seja, o limite superior é exclusivo
              NUTEMPO = case_when(NUTEMPO == "1" ~ "",
                                  NUTEMPO == "2" ~ "",
                                  NUTEMPO == "3" ~ "",
                                  NUTEMPO == "4" ~ "",
                                  !(NUTEMPO %in% c("1", "2", "3", "4", "9")) ~ "Outro",
                                  NUTEMPO == "9" ~ NA_character_)) %>% str()
  
# Juntar ao final do mutate anterior
  # cbo_df <- left_join(
  #   y = cbo, 
  #   by = c("ID_OCUPA_N" = "CODIGO") # *Altere "CODIGO" para o nome real da coluna do código de ocupação no CBO*
  # ) %>% str()


# 3. Criação da Nova Variável no DataFrame (df)
df <- df %>%
  mutate(
    FAIXA_ETARIA = cut(
      x = IDADE,                               # Variável numérica de entrada
      breaks = limites_sinan,                  # Vetor de limites (pontos de corte)
      labels = rotulos_sinan,                  # Vetor de rótulos (nomes das faixas)
      include.lowest = TRUE,                   # Inclui o limite inferior (0 ano)
      right = FALSE                            # Define o intervalo como [início, fim), ou seja, o limite superior é exclusivo
    )
  )

 str(cbo)                                    
              
colnames(df)

df$ID_OCUPA_N %>% unique()
