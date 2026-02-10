library(foreign)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(stringdist)


# Carregar todos os arquivos DBF ------------------------------------------

## Lista de arquivos DBF da pasta
arquivos <- list.files(path = "Dados_brutos",
                       pattern = "\\.dbf$",
                       ignore.case = TRUE)

## Função para ler os arquivos com campos de interesse
processa_arquivo <- function(arquivos){
  
  df_ini <- read.dbf(file.path("Dados_brutos", arquivos)) %>% 
    
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
}

###########################################################################
# LEITURA DE TODOS

lista_arquivos <- lapply(arquivos, processa_arquivo)




###########################################################################
# CONCATENAR BANCOS INTEIROS

df_todos <- Reduce(x = lista_arquivos, f = rbind.data.frame)   
  
  

###########################################################################
# TRATAR CAMPOS DE INTERESSE

## Seleciona apenas nomes das colunas de interesse
cols_alvo <- grep("^(AGENTE|P_ATIVO)", names(df_todos), value = TRUE, ignore.case = TRUE)


# Normalizacao dos dados --------------------------------------------------

## Letra minuscula
df_todos[cols_alvo] <- lapply(df_todos[cols_alvo], stringr::str_to_lower)

## Remover caracteres especiais e acentos
df_todos[cols_alvo] <- lapply(df_todos[cols_alvo], function(x) {
  stringi::stri_trans_general(str = x, id = "Latin-ASCII")
})

## Remover espacial de inicio e final
df_todos[cols_alvo] <- lapply(df_todos[cols_alvo] , stringr::str_trim)

## Substitui multiplos espacos por apenas um
df_todos[cols_alvo] <- lapply(df_todos[cols_alvo], stringr::str_squish)

## Substitui de caracteres de pontuação
df_todos[cols_alvo] <- lapply(df_todos[cols_alvo], function(x) {
  str_replace_all(x, "[-_/]", " ")
})


###########################################################################
# SALVAR DF CONCATENADO

saveRDS(object = df_todos,
        file = "01_df_concat_sem_filtro.rds")





  
  
  
  
  
  
