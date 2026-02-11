library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
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

## Substitui de caracteres de pontuação
df_ini[cols_alvo] <- lapply(df_ini[cols_alvo], function(x) {
  str_replace_all(x, "[-_/]", " ")
})


# Nomes comerciais --------------------------------------------------------

nome_produto <- unique(tolower(c(
  # nomes e variações corretos
  "paraquat", "gramoxone", "gramocil", "flak 200", "helmoxone", "laredo",
  "nuquat", "orbit", "paradox", "pramato", "quatdown", "sprayquat", "tocha",
  
  # -- erros e variações observadas
  "paroquat","paraquato","paraquatu","paracoate","parocoate","paroquate",
  "paraquate","paraquante","paraquant","parakuat","paracuat","paraquat 200",
  "paraquat sl","para quat","paraquatium","gramaxone","gramachone","gramoxin",
  "gramoxona","gramucil","granucil","gramuciu","gromaxone","gramonone",
  "granoxine","gromocil","gramozone","gramocile","gramociel","gramecil",
  "granocil","gramexil","gramokil","gromaxon","gramozoni",
  "herbicida paraquat","herbicida gramoxone","herbicida gramocil",
  "agrotoxico paraquat", "veneno paraquat", "veneno paraquate",
  
  # --erros gerados por algoritmo (plausiveis e realistas) ---
      # omissões e transposições simples
  "paraqut", "paraquar", "paraqaut", "parauat", "parquat",
      # trocas fonéticas / consoantes próximas
  "parakwat", "paraqwat", "parakuat", "parakuat", "paracuat",
      # pequenos erros de digitacao
  "paraquatd", "paraquatt", "paraqvat",
      # omissão de letra interna
  "paraqua", "paraqu",
      # gramoxone/gramocil foneticos
  "gramozon", "gramoxon", "gramoxoni", "gramocill", "gramociel", "gramocill",
  "gromaxon", "gromoxone", "gramon", "gramocie", "gramokson"
)))


# FILTRO ------------------------------------------------------------------

## Filtro



## Filtro definitivo
df_filtrado <- df_ini %>%
  filter(
    if_any(
      starts_with("AGENTE"),
      ~ (.x %in% nome_produto) |
        (stringr::str_detect(.x, 
                             stringr::regex(paste0("\\b(", paste(nome_produto, collapse="|"), ")\\b"), ignore_case=TRUE)))
    )
  )

## Conferencia
nrow(df_filtrado)

df_filtrado[cols_alvo] %>% 
  group_by(AGENTE_TOX) %>% 
  count()



# Old ---------------------------------------------------------------------


### Funcao para detectar semelhancas com distância de Levenshtein 
# `%~in%` <- function(x, table, method = "lv", maxDist = 2) {
#   !is.na(amatch(x, table, method = method, maxDist = maxDist))
# }
# ## Filtro Simples
# df_filtrado_simples <- df_ini[cols_alvo] %>% 
#   filter(if_any(c(starts_with("AGENTE"), starts_with("P_ATIVO")),
#                 ~ .x %in% nome_produto))