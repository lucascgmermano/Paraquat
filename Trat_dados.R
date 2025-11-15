library(dplyr)
library(lubridate)
library(stringr)
library(stringi)

df <- readRDS("df_todos.rds")

dplyr::glimpse(df)
str(df)

unique(df$NU_ANO)

df %>% mutate(across(.cols = c("NU_ANO","ANO_NASC"), .fns = as.factor))

str()