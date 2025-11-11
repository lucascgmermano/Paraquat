library(dplyr)
library(stringr)
library(stringi)
library(stringdist)
library(foreign)

# Carregar bancos ---------------------------------------------------------

df_ini <- read.dbf("Dados_brutos/IEXOBR11.dbf")

  ## Converter todo o dataframe para utf8
df_ini <- as.data.frame(
  lapply(df_ini, \(x)
         if (is.character(x) || is.factor(x))
           stri_enc_toutf8(as.character(x))
         else
           x
  ),
  stringsAsFactors = FALSE
)

  ## Seleciona colunas de interesse
cols_alvo <- grep("^(AGENTE|P_ATIVO)", names(df_ini), value = TRUE, ignore.case = TRUE)


# Normalizacao dos dados --------------------------------------------------

## Letra minuscula
df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], stringr::str_to_lower)
df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], function(x) {
  stringi::stri_trans_general(str = x, id = "Latin-ASCII")
})


# -------------------------------------------------------------------------

# FILTRO ------------------------------------------------------------------

## Funcao para pesquisa aproximada (cria operador aproximado)

`%~in%` <- function(x, table, method = "lv", maxDist = 2) {
  !is.na(amatch(x, table, method = method, maxDist = maxDist))
}


### Filtro definitivo (busca direta e indireta)
df_ini %>%
  filter(if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
                ~ .x %in% nome_produto) |
           if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
                  ~ .x %~in% nome_produto)) %>% View



stringi::stri_trans_general(str = df_ini$AGENTE_1, "Latin-ASCII")
