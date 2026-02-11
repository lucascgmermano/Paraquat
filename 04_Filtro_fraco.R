library(dplyr)

df <- readRDS("Dados_processados/01_df_concat_sem_filtro.rds")




# Filtro nome exato
nome_exato = c("paraquat", "gramoxone", "gramocil", "flak 200", "helmoxone", "laredo",
               "nuquat", "orbit", "paradox", "pramato", "quatdown", "sprayquat", "tocha")

df_1 <- df %>% 
  filter(
    if_any(
      starts_with("AGENTE") | starts_with("P_ATIVO"),
      ~ (.x %in% nome_exato)))


# Visualizar total
df_1 %>% nrow() # 1939 registros para 2007 a 2025


# Salvar ------------------------------------------------------------------

write.xlsx(x = df_1,
           file = "Dados_processados/df_filtro_simples.xlsx")
