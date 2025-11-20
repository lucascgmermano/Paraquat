library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(readr)


# Leitura dos dados -------------------------------------------------------

df <- readRDS("df_todos.rds")
cbo <- read.csv2(file = "CBO-Ocupacao.csv",
                 encoding = "latin1",
                 sep = "\t") %>% 
  mutate(CODIGO = as.character(CODIGO))

# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvs2024br.def
pop <- read.csv2(
  file = "POP2025_ibge_cnv.csv",
  sep = "\t",
  encoding = "latin1",
  colClasses = c("ID_MN_RESI" = "character")
)


# Faixas etarias
# Definição dos Limites (Breaks)
# 0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80 e 120 (como limite superior)
limites_sinan <- c(0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf)

# Definição dos Rótulos (Labels)
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



# Traformacao de classes e recodificacao ----------------------------------


df <- df %>% mutate(across(.cols = c("NU_ANO","ANO_NASC"), .fns = as.factor),
              
              SEM_NOT = as.factor(substr(x=SEM_NOT, start = 5, stop = 6)),
              
              SEM_PRI = as.factor(substr(x=SEM_PRI, start = 5, stop = 6)),
              
              IDADE = as.numeric(substr(x=NU_IDADE_N, start = 2, stop = 4)),
              
              IDADE = case_when(IDADE < 1 ~ 0,
                                IDADE >= 1 ~ IDADE),
              
              CS_GESTANT = recode(CS_GESTANT,
                                  "1" = "1 semestre",
                                  "2" = "2 semestre",
                                  "3" = "3 semestre",
                                  "4" = "Idade gestacional ign",
                                  "5" = "Nao",
                                  "6" = "Nao se aplica ",
                                  "9" = NA_character_),
              
              CS_RACA = recode(CS_RACA,
                               "1" = "Branca",
                               "2" = "Preta",
                               "3" = "Amarela",
                               "4" = "Parda",
                               "5" = "Indigena",
                               "9" = NA_character_),
              
              CS_ESCOL_N = recode(CS_ESCOL_N,
                                  "00" = "Analfabeto",
                                  "01" = "1 a 4 incompleto",
                                  "02" = "4a completa",
                                  "03" = "5 a 8 incompleto",
                                  "04" = "Fundamental completo",
                                  "05" = "Medio incompleto",
                                  "06" = "Medio completo",
                                  "07" = "Superior incompleto",
                                  "08" = "Superior completo",
                                  "10" = "Nao se aplica",
                                  "09" = NA_character_),
              
              FAIXA_ETARIA = cut(x = IDADE,                  # Ja retorna valores em factor
                                 breaks = limites_sinan,
                                 labels = rotulos_sinan,
                                 include.lowest = TRUE,      # Inclui o limite inferior (0 ano)
                                 right = FALSE),              # Define o intervalo como [início, fim), ou seja, o limite superior é exclusivo
              
              AGENTE_TOX = recode(.x = AGENTE_TOX,
                                  "01" = "Medicamento",
                                  "02" = "Agrotoxico agricola",
                                  "03" = "Agrotoxico domestico",
                                  "04" = "Agrotoxico spublica",
                                  "05" = "Raticida",
                                  "06" = "Produto veterinario",
                                  "07" = "Produto domiciliar",
                                  "08" = "Cosmetico",
                                  "09" = "Produto industrial",
                                  "10" = "Metal",
                                  "11" = "Drogas abuso",
                                  "12" = "Planta toxica",
                                  "13" = "Alimento bebida",
                                  "14" = "Outro",
                                  "99" = NA_character_,
                                  .default = NA_character_), # Opcional: define um valor para códigos não mapeados
              
              SIT_TRAB = recode(.x = SIT_TRAB,
                                "01" = "Empregado com carteira assinada",
                                "02" = "Empregado não registrado",
                                "03" = "Autonomo",
                                "04" = "Servidor publico estatutario",
                                "05" = "Servidor publlico celetista",
                                "06" = "Aposentado",
                                "07" = "Desempregado",
                                "08" = "Trabalho temporario",
                                "09" = "Cooperativado",
                                "10" = "Trabalhador avulso",
                                "11" = "Empregador",
                                "12" = "Outro",
                                "99" = NA_character_,
                                .default = NA_character_),
              
              LOC_EXPO = recode(.x = LOC_EXPO,
                                "1" = "Residencia",
                                "2" = "Ambiente de trabalho",
                                "3" = "Trajeto do trabalho",
                                "4" = "Servicos de saude",
                                "5" = "Escola/creche",
                                "6" = "Ambiente externo",
                                "7" = "Outro",
                                "9" = NA_character_,
                                .default = NA_character_),
              
              UTILIZACAO = recode(.x = UTILIZACAO,
                                  "1" = "Inseticida",
                                  "2" = "Herbicida",
                                  "3" = "Carrapaticida",
                                  "4" = "Raticida",
                                  "5" = "Fungicida",
                                  "6" = "Preservante para madeira",
                                  "7" = "Outro",
                                  "8" = "Nao se aplica",
                                  "9" = NA_character_,
                                  .default = NA_character_),
              
              ATIVIDA_1 = recode(.x = ATIVIDA_1,
                                 "01" = "Diluicao",
                                 "02" = "Pulverizacao",
                                 "03" = "Tratamento de sementes",
                                 "04" = "Armazenagem",
                                 "05" = "Colheita",
                                 "06" = "Transporte",
                                 "07" = "Desinsetizacao",
                                 "08" = "Producao/formulacao",
                                 "09" = "Outros",
                                 "10" = "Nao se aplica",
                                 "99" = NA_character_,
                                 .default = NA_character_),
              
              ATIVIDA_2 = recode(.x = ATIVIDA_2,
                                 "01" = "Diluicao",
                                 "02" = "Pulverizacao",
                                 "03" = "Tratamento de sementes",
                                 "04" = "Armazenagem",
                                 "05" = "Colheita",
                                 "06" = "Transporte",
                                 "07" = "Desinsetizacao",
                                 "08" = "Producao/formulacao",
                                 "09" = "Outros",
                                 "10" = "Nao se aplica",
                                 "99" = NA_character_,
                                 .default = NA_character_),
              
              ATIVIDA_3 = recode(.x = ATIVIDA_3,
                                 "01" = "Diluicao",
                                 "02" = "Pulverizacao",
                                 "03" = "Tratamento de sementes",
                                 "04" = "Armazenagem",
                                 "05" = "Colheita",
                                 "06" = "Transporte",
                                 "07" = "Desinsetizacao",
                                 "08" = "Producao/formulacao",
                                 "09" = "Outros",
                                 "10" = "Nao se aplica",
                                 "99" = NA_character_,
                                 .default = NA_character_),
              
              VIA_1 = recode(.x = VIA_1,
                             "1" = "Digestiva",
                             "2" = "Cutanea",
                             "3" = "Respiratoria",
                             "4" = "Ocular",
                             "5" = "Parenteral",
                             "6" = "Vaginal",
                             "7" = "Transplacentaria",
                             "8" =  "Outra",
                             "9" = NA_character_,
                             .default = NA_character_),
              
              VIA_2 = recode(.x = VIA_2,
                             "1" = "Digestiva",
                             "2" = "Cutanea",
                             "3" = "Respiratoria",
                             "4" = "Ocular",
                             "5" = "Parenteral",
                             "6" = "Vaginal",
                             "7" = "Transplacentaria",
                             "8" =  "Outra",
                             "9" = NA_character_),
              
              VIA_3 = recode(.x = VIA_3,
                             "1" = "Digestiva",
                             "2" = "Cutanea",
                             "3" = "Respiratoria",
                             "4" = "Ocular",
                             "5" = "Parenteral",
                             "6" = "Vaginal",
                             "7" = "Transplacentaria",
                             "8" =  "Outra",
                             "9" = NA_character_),
              
              CIRCUNSTAN = recode(.x = CIRCUNSTAN,
                                  "01" = "Uso habitual",
                                  "02" = "Acidental",
                                  "03" = "Ambiental",
                                  "04" = "Uso terapeutico",
                                  "05" = "Prescricao medica inadequada",
                                  "06" = "Erro de administracao",
                                  "07" = "Automedicacao",
                                  "08" = "Abuso",
                                  "09" = "Ingestao de alimento ou bebida",
                                  "10" = "Tentativa de suicidio",
                                  "11" = "Tentativa de aborto",
                                  "12" = "Violencia/homicidio",
                                  "13" = "Outra",
                                  "99" = NA_character_),
              
              DOENCA_TRA = recode(.x = DOENCA_TRA,
                                  "1" = "Sim",
                                  "2" = "Nao",
                                  "9" = NA_character_),
              
              TPEXP = recode(.x = TPEXP,
                             "1" = "Aguda unica",
                             "2" = "Aguda repetida",
                             "3" = "Cronica",
                             "4" = "Aguda sobre cronica",
                             "9" = NA_character_),
              
              TPTEMPO = recode(.x = TPTEMPO,
                               "1" = "Hora",
                               "2" = "Dia",
                               "3" = "Mes",
                               "4" = "Ano",
                               "9" = NA_character_),
              
              NUTEMPO = recode(.x = NUTEMPO,
                               "1" = "Hora",
                               "2" = "Dia",
                               "3" = "Mes",
                               "4" = "Ano",
                               "9" = NA_character_),
              
              TPATENDE = recode(.x = TPATENDE,
                                "1" = "Hospitalar",
                                "2" = "Ambulatorial",
                                "3" = "Domiciliar",
                                "4" = "Nenhum",
                                "9" = NA_character_),
              
              HOSPITAL = recode(.x = HOSPITAL,
                                "1" = "Sim",
                                "2" = "Nao",
                                "9" = NA_character_),
              
              CLASSI_FIN = recode(.x = CLASSI_FIN,
                                  "1" = "Intoxicacao confirmada",
                                  "2" = "Exposicao",
                                  "3" = "Reacao adversa",
                                  "4" = "Diagnostico diferencial",
                                  "5" = "Sindrome de abstinencia",
                                  "9" = NA_character_),
              
              CRITERIO = recode(.x = CRITERIO,
                                "1" = "Clinico laboratorial",
                                "2" = "Clinico epidemiologico",
                                "3" = "Clinico"),
              
              EVOLUCAO = recode(.x = EVOLUCAO,
                                "1" = "Cura sem sequela",
                                "2" = "Cura com sequela",
                                "3" = "Obito por intoxicacao exogena",
                                "4" = "Obito por outra causa",
                                "5" = "Perda do seguimento",
                                "9" = NA_character_),
              
              CAT = recode(.x = CAT,
                           "1" = "Sim",
                           "2" = "Nao",
                           "3" = "Nao se aplica",
                           "9" = NA_character_))
              

# Criação da Nova Variável no DataFrame (df)
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

# Juntar dataframe com CBO
  df_cbo <- left_join(x = df, y = cbo,
    by = c("ID_OCUPA_N" = "CODIGO"))

# Renomear coluna cbo
  df_cbo <- df_cbo %>%
    rename(CBO = TITULO)
  
  
# Juntar dataframe com pop
df_final <- left_join(x = df_cbo,
                      y = pop)

