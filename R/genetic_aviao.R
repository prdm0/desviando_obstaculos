
ga <- function(
    array_melhor_w_1,
    array_melhor_w_2,
    crossover_by_col = TRUE,
    prop = 0.2,
    prob_mutation = 0.1) {
  
  # prob refere-se a proporção de linhas ou colunas que serão trocadas
  
  # Dois melhores w_1:
  w_1_melhor <- array_melhor_w_1[ , , 1L]
  w_1_pior <- array_melhor_w_1[ , , 2L]
  
  # Dois melhores w_2:
  w_2_melhor <- array_melhor_w_2[ , , 1L]
  w_2_pior <- array_melhor_w_2[ , , 2L]
  
  dim_w_1 <- dim(w_1_melhor)
  dim_w_2 <- dim(w_2_melhor)
  
  # O crossover ocorrerá trocando linhas ou colunas entre os melhores w_1. O mesmo
  # para w_2
  if(crossover_by_col) {
    # Número de colunas das matrizes
    numero_colunas_w1 <- round(quantile(sample(1L:dim_w_1[2L]), probs = prop))
    numero_colunas_w2 <- round(quantile(sample(1L:dim_w_2[2L]), probs = prop))
    
    colunas_w1 <- sort(sample(x = 1L:dim_w_1[2L], size = numero_colunas_w1, replace = FALSE)) 
    w_1_melhor[ , colunas_w1] <- w_1_pior[ , colunas_w1]
    
    colunas_w2 <- sort(sample(x = 1L:dim_w_2[2L], size = numero_colunas_w2, replace = FALSE))
    w_2_melhor[ , colunas_w2] <- w_2_pior[ , colunas_w2]
  } else {
    # Número de linhas das matrizes
    numero_linhas_w1 <- round(quantile(sample(1L:dim_w_1[1L]), probs = prop))
    numero_linhas_w2 <- round(quantile(sample(1L:dim_w_2[1L]), probs = prop))

    linhas_w1 <- sort(sample(x = 1L:dim_w_1[1L], size = numero_linhas_w1, replace = FALSE)) 
    w_1_melhor[linhas_w1 , ] <- (w_1_pior[linhas_w1 , ] + w_1_melhor[linhas_w1 , ])/2
    
    linhas_w2 <- sort(sample(x = 1L:dim_w_2[1L], size = numero_linhas_w2, replace = FALSE))
    w_2_melhor[linhas_w2 , ] <- (w_2_pior[linhas_w2 , ] + w_2_melhor[linhas_w2 , ])/2
  }
  
  # A mutação ocorrerá trocando o sinal de uma linha de w_1 e w_2
  if(sample(c(TRUE, FALSE), size = 1L, prob = c(prob_mutation, 1 - prob_mutation))){
    linha <- sample(1L:dim_w_1[1L], size = 2L)
    w_1_melhor[linha ,] <- -1 * w_1_melhor[linha , ] |> tanh()
    w_2_melhor[linha ,] <- -1 * w_2_melhor[linha , ] |> tanh()
  }
  
  return(list(ga_w_1 = w_1_melhor, ga_w_2 = w_2_melhor))
}
