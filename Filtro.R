library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(tidymodels)

# Carregar bancos ---------------------------------------------------------

df_ini <- read.dbf("Dados_brutos/IEXOBR11.dbf")




# Agentes em minusculas ---------------------------------------------------


df_ini <- df_ini %>%
  mutate(
    across(
      c(starts_with("AGENTE"), starts_with("P_ATIVO")),
      ~ tolower(as.character(.x))))
    


# Nomes comerciais --------------------------------------------------------


nomes_comerciais <- c("laredo",
                       "orbit",
                       "tocha",
                       "gramocil",
                       "gramoxone",
                       "helmoxone",
                       "paradox",
                       "pramato",
                       "gramoking",
                       "quatdown",
                       "sprayquat",
                       "nuquat",
                       "flak 200",
                       "paraquat")

df_ini %>%
  filter(if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
                ~ .x %in% nomes_comerciais)) %>% length()


