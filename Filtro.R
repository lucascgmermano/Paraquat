library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringdist)

# Carregar bancos ---------------------------------------------------------

df_ini <- read.dbf("Dados_brutos/IEXOBR11.dbf")

    ## Converter todo o dataframe para utf8
    df_ini <- as.data.frame(
      lapply(df_ini, \(x)
             if (is.character(x) || is.factor(x))
               stri_enc_toutf8(as.character(x))
             else
               x), stringsAsFactors = FALSE)
    
    ## Seleciona apenas nomes das colunas de interesse
    cols_alvo <- grep("^(AGENTE|P_ATIVO)", names(df_ini), value = TRUE, ignore.case = TRUE)


# Normalizacao dos dados --------------------------------------------------

    ## Letra minuscula
    df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], stringr::str_to_lower)
    
    ## Remover caracteres especiais e acentos
    df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], function(x) {
      stringi::stri_trans_general(str = x, id = "Latin-ASCII")
    })
    
    ## Remover espacial de inicio e final
    df_ini[cols_alvo] <- lapply(df_ini[cols_alvo] , stringr::str_trim)

    ## Substitui multiplos espacos por apenas um
    df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], stringr::str_squish)

    
# Nomes comerciais --------------------------------------------------------

nome_produto <- c("c12h14n2",
                   "bipiridínio",
                   "bipyridinium",
                   "flak 200",
                   "gramocil",
                   "gramoking",
                   "gramoking",
                   "gramoxone",
                   "gramoxone 200",
                   "helmoxone",
                   "laredo",
                   "nuquat",
                   "orbit",
                   "paradox",
                   "paraquat",
                   "paraquat 200 sl",
                   "paraquate",
                   "paraquate alta 200 sl",
                   "pramato",
                   "quatdown",
                   "sprayquat",
                   "tocha"
                   )


# FILTRO ------------------------------------------------------------------

    ## Filtro complexo
    
    ### Funcao para detectar semelhancas com distância de Levenshtein 
`%~in%` <- function(x, table, method = "lv", maxDist = 1) {
  !is.na(amatch(x, table, method = method, maxDist = maxDist))
}


df_filtrado <- df_ini %>%
  filter(
    if_any(
      c(starts_with("AGENTE"), starts_with("P_ATIVO")),
      ~ (.x %in% nome_produto) | (.x %~in% nome_produto)
    )
  )

View(df_filtrado[cols_alvo])


  ## Filtro Simples
df_filtrado_simples <- df_ini[cols_alvo] %>% 
  filter(if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
              ~ .x %in% nome_produto))

View(df_filtrado_simples)
