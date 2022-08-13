source("R/funcoes_auxiliares.R")

treinamento <- 
  function(
    start_carro = c(20, 15),
    sensibilidade = 2L,
    fun = "gaussian",
    epocas = 100L,
    path = NULL
  ){
    
    if(start_carro[1L] < 6) stop("Escolhar um valor de 6<=x<=100 e 5<=y<=25")
    
    coordenada_atual_carro <<- start_carro
    
    one_step <- function(id_epoca){
      
      # Resetando variáveis globais
      movimento_aleatorio_buraco(
        sensibilidade = sensibilidade,
        w_1 = w_1,
        w_2 = w_2,
        fun = fun,
        graphic = FALSE,
        som = FALSE
      )

      # Iniciando a sequência de frames
      sucesso <- 1L 
      # Roda até o erro
      repeat{
        movimento_aleatorio_buraco(
          sensibilidade = sensibilidade,
          w_1 = w_1,
          w_2 = w_2,
          fun = fun,
          graphic = FALSE,
          som = FALSE
        )
        
        if(is.na(check_colisao(sensibilidade, coordenada_atual_carro, som = FALSE, graphic = FALSE)))
            break
        
        sucesso <- sucesso + 1L    
      } # Aqui termina o repeat

      if(sucesso > vetor_sucessos[1L]){
        array_w_1[ , , 1L] <<- w_1
        array_w_2[ , , 1L] <<- w_2
        vetor_sucessos[1L] <<- sucesso + 1L  
      }
      
      if(sucesso > vetor_sucessos[2L] && sucesso < vetor_sucessos[1L]){
        array_w_1[ , , 2L] <<- w_1
        array_w_2[ , , 2L] <<- w_2
        vetor_sucessos[2L] <<- sucesso + 1L 
      }
      
      genetic <- ga(array_w_1, array_w_2, prop = 0.7, prob_mutation = 0.2)
      w_1 <<- genetic$ga_w_1
      w_2 <<- genetic$ga_w_2

    } # Aqui termina a função one_step()
    
    pbsapply(X = 1:epocas, FUN = one_step)
    
    melhor_w_1 <- array_w_1[ , , 1L]
    melhor_w_2 <- array_w_2[ , , 2L]
    
    # Salvando a melhor rede (o melhor conjunto de pesos sináptico)
    if(!is.null(path)){
       save(melhor_w_1, file = paste0(path, "/w_1.RData"))
       save(melhor_w_2, file = paste0(path, "/w_2.RData"))
    }
} # Aqui termina a função treinamento

set.seed(0)

n_neuronios <- 6L

# Aleatorizando as matrizes de pessos
array_w_1 <- array(data = NA, dim = c(n_neuronios, n_neuronios, 2L)) # dois melhores
array_w_2 <- array(data = NA, dim = c(n_neuronios, 3L, 2L)) # dois melhores

# Carregando pesos iniciais
load(file = "rede_treinada/w_1_backup.RData")
load(file = "rede_treinada/w_2_backup.RData")
w_1 <- melhor_w_1 
w_2 <- melhor_w_2

array_w_1[ , , 1L] <- w_1
array_w_2[ , , 1L] <- w_2
array_w_1[ , , 2L] <- w_1
array_w_2[ , , 2L] <- w_2

vetor_sucessos <- c(1L, 1L) 

treinamento(
  start_carro = c(20, 15),
  sensibilidade = 2L,
  fun = "gaussian",
  epocas = "100000",
  path = "rede_treinada/"
)
