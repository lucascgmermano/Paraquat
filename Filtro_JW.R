library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringdist)
library(stringi)

# Carregar bancos ---------------------------------------------------------

df_ini <- read.dbf("Dados_brutos/IEXOBR11.dbf")

## Converter todo o dataframe para utf8
df_ini <- as.data.frame(
  lapply(df_ini, \(x)
         if (is.character(x) || is.factor(x))
           stringi::stri_enc_toutf8(as.character(x))
         else
           x), stringsAsFactors = FALSE)

## Seleciona apenas nomes das colunas de interesse
cols_alvo <- grep("^(AGENTE|P_ATIVO)", names(df_ini), value = TRUE, ignore.case = TRUE)

## Recodifica categorias de agente toxico
df_ini$AGENTE_TOX <- recode(
  .x = df_ini$AGENTE_TOX,
  "01" = "Medicamento",
  "02" = "Agrotoxico agricola",
  "03" = "Agrotoxico domestico",
  "04" = "Agrotoxico spublica",
  "05" = "Raticida",
  "06" = "Produto veterinario",
  "07" = "Produto domiciliar",
  "08" = "Cosmetico",
  "09" = "Produto industrial", # Ajustar se houver continuação
  "10" = "Metal",
  "11" = "Drogas abuso",
  "12" = "Planta toxica",
  "13" = "Alimento bebida",
  "14" = "Outro",
  "99" = "Ignorado",
  .default = NA_character_ # Opcional: define um valor para códigos não mapeados
)

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

nome_produto <- c("bipiridínio",
                  "bipyridinium",
                  "flak 200",
                  "flak",
                  "gramocil",
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
`%~in%` <- function(x, table, method = "jw", maxDist = 0.15) {
  !is.na(amatch(x, table, method = method, maxDist = maxDist))
}

df_jw <- df_ini %>%
  filter(
    if_any(
      c(starts_with("AGENTE"), starts_with("P_ATIVO")),
      ~ (.x %in% nome_produto) |
        (.x %~in% nome_produto) |
        str_detect(.x, regex(paste(nome_produto, collapse = "|"), ignore_case = TRUE))
    )
  )


df_jw %>% nrow()

df_jw %>% 
  group_by(AGENTE_TOX) %>% 
  summarise(n())

View(df_jw[cols_alvo])

## Filtro Simples
df_filtrado_simples <- df_ini[cols_alvo] %>% 
  filter(if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
                ~ .x %in% nome_produto))

df_filtrado_simples %>% nrow()

View(df_filtrado_simples[cols_alvo])
