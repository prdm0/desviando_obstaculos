source("funcoes_auxiliares_paralelo.R")

treinamento <- 
  function(
    start_carro = c(20, 15),
    n_neuronios = 6L,
    sensibilidade = 2L,
    epocas = 10,
    path = NULL
  ){
    
    if(start_carro[1L] < 6) stop("Escolhar um valor de 6<=x<=100 e 5<=y<=25")
    
    # Aleatorizando as matrizes de pessos
    array_w_1 <- array(data = NA, dim = c(n_neuronios, n_neuronios, 2L)) # dois melhores
    array_w_2 <- array(data = NA, dim = c(n_neuronios, 3L, 2L)) # dois melhores
    
    # número de sucessos dos dois melhores
    
    # c(primeiro_melhor, segundo_melhor)
    vetor_sucessos <- c(1L, 1L)
    
    w_1 <- matrix(runif(n = n_neuronios * n_neuronios, 5, 10), ncol = n_neuronios, nrow = n_neuronios)
    w_2 <- matrix(runif(n = n_neuronios * 3L, 5, 10), ncol = 3L, nrow = n_neuronios)
    
    array_w_1[ , , 1L] <- matrix(rnorm(n = n_neuronios * n_neuronios, sd = 2), ncol = n_neuronios, nrow = n_neuronios)
    array_w_2[ , , 1L] <- matrix(rnorm(n = n_neuronios * 3L), ncol = 3L, nrow = n_neuronios)
    array_w_1[ , , 2L] <- matrix(rnorm(n = n_neuronios * n_neuronios), ncol = n_neuronios, nrow = n_neuronios) 
    array_w_2[ , , 2L] <- matrix(rnorm(n = n_neuronios * 3L), ncol = 3L, nrow = n_neuronios)
    
    coordenada_atual_carro <- start_carro

    one_step <- function(id_epoca){
      
      # Resetando variáveis globais
      coords <- 
        movimento_aleatorio_buraco(
          sensibilidade = sensibilidade,
          w_1 = w_1,
          w_2 = w_2,
          coordenada_atual_carro = coordenada_atual_carro
      )
      
      coordenada_atual_carro <- coords[[1L]]
      coordenada_atual_buraco <- coords[[2L]]
      
      # Iniciando a sequência de frames
      sucesso <- 1L 
      # Roda até o erro
      repeat{
        coords <- 
          movimento_aleatorio_buraco(
            sensibilidade = sensibilidade, 
            w_1 = w_1,
            w_2 = w_2,
            coordenada_atual_carro = coordenada_atual_carro
        )
        
        coordenada_atual_carro <- coords[[1L]]
        coordenada_atual_buraco <- coords[[2L]]
         
        if(is.na(check_colisao(sensibilidade, coordenada_atual_carro = coordenada_atual_carro, coordenada_atual_buraco = coordenada_atual_buraco)))
          break
        
        sucesso <- sucesso + 1L    
      } # Aqui termina o repeat
      
      if(sucesso > vetor_sucessos[1L]){
        array_w_1[ , , 1L] <- w_1
        array_w_2[ , , 1L] <- w_2
        vetor_sucessos[1L] <- sucesso + 1L  
      }
      
      if(sucesso > vetor_sucessos[2L] && sucesso < vetor_sucessos[1L]){
        array_w_1[ , , 2L] <- w_1
        array_w_2[ , , 2L] <- w_2
        vetor_sucessos[2L] <- sucesso + 1L 
      }
      
      genetic <- ga(array_w_1, array_w_2, prop = 0.2, prob_mutation = 0.2)
      w_1 <- genetic$ga_w_1
      w_2 <- genetic$ga_w_2
      
      return(list(vetor_sucessos = vetor_sucessos, array_w_1 = array_w_1[,,1L], array_w_2 = array_w_2[,,1L]))
      
    } # Aqui termina a função one_step()
    
    result <- pbmcapply::pbmclapply(X = 1L:epocas, FUN = one_step, mc.cores = parallel::detectCores())
    
    melhor_w_1 <- result[[max_list(result)]]$array_w_1
    melhor_w_2 <- result[[max_list(result)]]$array_w_2
    sucessos <- result[[max_list(result)]]$vetor_sucessos
    
    # Salvando a melhor rede (o melhor conjunto de pesos sináptico)
    if(!is.null(path)){
      save(melhor_w_1, file = paste0(path, "/w_1.RData"))
      save(melhor_w_2, file = paste0(path, "/w_2.RData"))
    }
    
    # return(list(
    #     vetor_sucessos = sucessos,
    #     melhor_w_1 = melhor_w_1,
    #     melhor_w_2 = melhor_w_2
    #   )
    # )
    
    result
  } # Aqui termina a função treinamento

set.seed(0)

treinamento(
  start_carro = c(20, 15),
  sensibilidade = 2L,
  epocas = 1000L,
  path = "rede_treinada/"
) -> a

