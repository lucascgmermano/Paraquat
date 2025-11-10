library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(tidymodels)


# Carregar bancos ---------------------------------------------------------

dados_ini <- read.dbf("Dados_brutos/IEXOBR19.dbf")

dados_ini$AGENTE_1 <- str_to_lower(dados_ini$AGENTE_1)
dados_ini$AGENTE_2 <- str_to_lower(dados_ini$AGENTE_2)
dados_ini$AGENTE_3 <- str_to_lower(dados_ini$AGENTE_3)
dados_ini$P_ATIVO_1 <- str_to_lower(dados_ini$P_ATIVO_1)
dados_ini$P_ATIVO_2 <- str_to_lower(dados_ini$P_ATIVO_2)
dados_ini$P_ATIVO_2 <- str_to_lower(dados_ini$P_ATIVO_3)

nomes_comerciais <- c("alamos","atanor","ccab","china","chn","diquat","flak","flak 200",
                      "gramocil","gramocil super","gramoking","gramoxone","gramuron","helm","helmoxone",
                      "herbipar","laredo","nufarm","nuquat","orbit","ouro fino","paracuat","paradox",
                      "paraquat","paraquat sl","paraquate","pramato","quatdown","rainbow","sinon",
                      "sprayquat","syngenta","tocha","yn","zeneca","zy",
                      "para-quat","para quate","gramoxon","gramo","paraq","quat","syng","nortox","gromoxone","paracoate")

x <- dados_ini %>% filter(AGENTE_1 %in% nomes_comerciais |
                            AGENTE_2 %in% nomes_comerciais |
                            AGENTE_3 %in% nomes_comerciais |
                            P_ATIVO_1 %in% nomes_comerciais |
                            P_ATIVO_2 %in% nomes_comerciais |
                            P_ATIVO_3 %in% nomes_comerciais )
x$AGENTE_1


# -------------------------------------------------------------------------


library(dplyr)
library(stringr)

# sua lista de nomes comerciais e variantes
nomes_comerciais <- c(
  "alamos","atanor","ccab","china","chn","diquat","flak","flak 200",
  "gramocil","gramocil super","gramoking","gramoxone","gramuron","helm","helmoxone",
  "herbipar","laredo","nufarm","nuquat","orbit","ouro fino","paracuat","paradox",
  "paraquat","paraquat sl","paraquate","pramato","quatdown","rainbow","sinon",
  "sprayquat","syngenta","tocha","yn","zeneca","zy",
  "para-quat","para quate","gramoxon","gramo","paraq","quat","syng","nortox","gromoxone","paracoate"
)

# junta tudo em uma expressão regular com "ou" (|)
padrao_regex <- paste(nomes_comerciais, collapse = "|")

# aplica busca em qualquer campo (sem distinção de maiúsculas/minúsculas ou acentos)
x <- dados_ini %>%
  mutate(across(c(AGENTE_1, AGENTE_2, AGENTE_3, P_ATIVO_1, P_ATIVO_2, P_ATIVO_3),
                ~ str_to_lower(iconv(.x, to = "ASCII//TRANSLIT")))) %>%
  filter(if_any(c(AGENTE_1, AGENTE_2, AGENTE_3, P_ATIVO_1, P_ATIVO_2, P_ATIVO_3),
                ~ str_detect(.x, padrao_regex)))
x$AGENTE_1
