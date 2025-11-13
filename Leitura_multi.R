library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(stringdist)


arquivos <- list.files(path = "Dados_brutos",
                       pattern = "\\.dbf$",
                       ignore.case = TRUE)

###########################################################################
## FUNCAO
###########################################################################
# Carregar bancos ---------------------------------------------------------

processa_arquivo <- function(arquivo){

  df_ini <- read.dbf(file.path("Dados_brutos", arquivo)) %>% 
    
    select("DT_NOTIFIC", "SEM_NOT",  "NU_ANO", "SG_UF_NOT",
                      "ID_MUNICIP", "ID_REGIONA", "ID_UNIDADE", "DT_SIN_PRI",
                      "SEM_PRI", "ANO_NASC",  "NU_IDADE_N", "CS_SEXO",  
                      "CS_GESTANT", "CS_RACA", "CS_ESCOL_N", "SG_UF",
                      "ID_MN_RESI", "ID_RG_RESI", "ID_PAIS",  "DT_INVEST",
                      "ID_OCUPA_N", "SIT_TRAB",   "LOC_EXPO",   "CNAE","UF_EMP",
                      "MUN_EMP",  "PAIS_EXP", "AGENTE_TOX", "COAGTOXMA1",
                      "COAGTOXMA2", "COAGTOXMA3", "AGENTE_1", "AGENTE_2",  
                      "AGENTE_3", "P_ATIVO_1", "P_ATIVO_2", "P_ATIVO_3", 
                      "UTILIZACAO", "ATIVIDA_1",  "ATIVIDA_2",  "ATIVIDA_3",
                      "VIA_1", "VIA_2", "VIA_3", "CIRCUNSTAN", "DOENCA_TRA",
                      "TPEXP",  "NUTEMPO",  "TPTEMPO",  "TPATENDE", "HOSPITAL", 
                      "DTINTERNA", "UF_HOSP", "MUN_HOSP", "CLASSI_FIN",
                      "DIAG_CONF",  "CRITERIO", "EVOLUCAO", "DT_OBITO", "CAT",       
                      "DT_ENCERRA", "DT_DIGITA",  "DT_TRANSUS", "DT_TRANSDM",
                      "TRAB_DESC", "LOC_EXP_DE", "OUT_AGENTE", "UTIL_DESC",
                      "LAVOURA",  "CIRCUN_DES")
  

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
}

# n <- n+1 
# 
# lista_arquivos[[n]] <- *******
  
  
###########################################################################
# LEITURA DE TODOS

lista_arquivos <- lapply(arquivos, processa_arquivo)

###########################################################################
# JUNTAR TUDO

df_todos <- Reduce(x = lista_arquivos, f = rbind.data.frame) 


###########################################################################
# SALVAR DF

saveRDS(object = df_todos,
        file = "df_todos.rds")

