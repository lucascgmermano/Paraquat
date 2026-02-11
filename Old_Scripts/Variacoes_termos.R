library(stringdist)
library(stringr)
library(dplyr)

# ----------------------------------------------
# Função para gerar variações prováveis de palavras
# ----------------------------------------------
gerar_variacoes <- function(base_words, max_dist = 1, teclado_pt = TRUE) {
  
  # Substituições comuns no português e erros de teclado QWERTY
  substituicoes <- list(
    a = c("q","s","z"),
    e = c("w","r","3"),
    i = c("u","o","1","l"),
    o = c("p","i","0"),
    u = c("y","i","v"),
    q = c("w","a"),
    g = c("h","f","b"),
    m = c("n","b"),
    n = c("m","b"),
    x = c("z","s","c"),
    c = c("x","v"),
    r = c("t","e","f"),
    t = c("r","y","f","g"),
    p = c("o","l"),
    l = c("k","p"),
    s = c("a","x","z")
  )
  
  gerar_erros <- function(palavra) {
    chars <- unlist(strsplit(palavra, ""))
    variacoes <- c()
    
    # Omissão de letras
    for (i in seq_along(chars)) {
      variacoes <- c(variacoes, paste0(chars[-i], collapse = ""))
    }
    
    # Duplicação de letras
    for (i in seq_along(chars)) {
      variacoes <- c(variacoes, paste0(chars[1:i], chars[i], chars[(i+1):length(chars)], collapse = ""))
    }
    
    # Substituições por letras próximas
    for (i in seq_along(chars)) {
      if (chars[i] %in% names(substituicoes)) {
        for (sub in substituicoes[[chars[i]]]) {
          variacoes <- c(variacoes, paste0(replace(chars, i, sub), collapse = ""))
        }
      }
    }
    
    # Inserção de espaços e erros de separação
    variacoes <- c(variacoes, str_replace_all(palavra, "(?<=.{3})", " "))
    variacoes <- c(variacoes, str_replace_all(palavra, "(?<=.{4})", " "))
    
    unique(variacoes)
  }
  
  # Aplicar para todas as palavras-base
  variações <- base_words %>%
    tolower() %>%
    unique() %>%
    lapply(gerar_erros) %>%
    unlist() %>%
    unique()
  
  # Remover casos muito curtos e manter apenas strings próximas
  variações_filtradas <- variações[
    stringdist::stringdistmatrix(variações, base_words, method = "lv") <= max_dist + 2
  ]
  
  # Retornar base + variações plausíveis
  sort(unique(c(base_words, variações_filtradas)))
}

# ----------------------------------------------
# Uso prático: gerar variações para os nomes-base
# ----------------------------------------------
nomes_base <- c("paraquat", "gramoxone", "gramocil", "flak 200", "helmoxone", "nuquat", "pramato")

nome_produto_expandido <- gerar_variacoes(nomes_base, max_dist = 2)

# Visualizar alguns resultados
head(nome_produto_expandido, 40)
length(nome_produto_expandido)
