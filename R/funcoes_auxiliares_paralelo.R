# Bibliotecas
#library(png)
library(magick)
#library(bigstatsr)
library(beepr)
library(glue)
library(pbapply)
#library(furrr)
library(pbmcapply)

# Limopando memória
rm(list = ls(all = TRUE))

# Chamando arquivos
source("R/rede.R")
source("R/genetic.R")

# Os recrusos (entrada da rede):
#   1 - Posição atual do carro
#   2 - Posição do buraco
#   3 - Distância entre o carro e o buraco
#   Observação: A função que irá movimentar o carro irá receber essa três entradas

# As decisões (saída da rede):
#   1 - Para cima
#   2 - Para baixo
#   3 - Permanecer no mesmo local
#   Observação: A saíde da rede tomará uma das três decisões acima

# Variáveis Globais
raster_carro <- magick::image_read(path = "imagens/carro_menor.png") |> 
  magick::image_rotate(degrees = 90)
raster_buraco <- magick::image_read(path = "imagens/buraco_branco.png")

rip <- magick::image_read(path = "imagens/rip.png")

coordenada_atual_carro <- c(20, 15)
coordenada_atual_buraco <- double(length = 2L)
distancia_carro_buraco <- double(length = 1L)

check_colisao <- 
  function(sensibilidade = 2L,
           coordenada_atual_buraco,
           coordenada_atual_carro) {
  
  if(coordenada_atual_carro[2L] < 5 || coordenada_atual_carro[2L] > 25) return(NA)
  
  if(abs(coordenada_atual_carro[2L] - coordenada_atual_buraco[2L]) <= sensibilidade){
    toque_y <- TRUE  
  } else {
    toque_y <- FALSE
  }
  
  if(toque_y && coordenada_atual_buraco[1L] == coordenada_atual_carro[1L]){
    return(NA)
  } 
  return(-1L)
}

movimento_carro <- function(
    decisao, 
    sensibilidade = 2L,
    coordenada_atual_carro){
  
  switch (
    decisao,
    cima = {
      coordenada_atual_carro[2L] <- coordenada_atual_carro[2L] + sensibilidade
    },
    baixo = {
      coordenada_atual_carro[2L] <- coordenada_atual_carro[2L] - sensibilidade
    },
    parado = {
      coordenada_atual_carro[2L] <- coordenada_atual_carro[2L] + 0L
    }
  )
  
  if(coordenada_atual_carro[2L] > 25) coordenada_atual_carro[2L] <- 25
  if(coordenada_atual_carro[2L] < 8) coordenada_atual_carro[2L] <- 8

  return(coordenada_atual_carro)
}

movimento_aleatorio_buraco <- 
  function(sensibilidade, 
           w_1, 
           w_2,
           coordenada_atual_carro
  ){
    
    # Coordenadas aleatóra do buraco
    coordenada_atual_buraco <- 
      c(
        sample(x = (coordenada_atual_carro[1L] + 5L):100L, size = 1L),
        sample(x = 7:25L, size = 1L)
      )
    
    sucesso <- 1L
    
    # Não poderá ser feita por funcional, uma vez que contém a aleatoriedade 
    # da colisão, que poderá eventualmente ocorrer.
    for(x_buraco in coordenada_atual_buraco[1L]:coordenada_atual_carro[1L]){

      # Desenhando um buraco e salvando na variável global a posição 
      # atual do buraco.
      coordenada_atual_buraco <- c(x_buraco, coordenada_atual_buraco[2L])
      
      recursos <- 
        c(
          1L,
          coordenada_atual_carro[1L],
          coordenada_atual_carro[2L],
          coordenada_atual_buraco[1L],
          coordenada_atual_buraco[2L],
          dist(rbind(coordenada_atual_buraco, coordenada_atual_carro))
        )
      
      coordenada_atual_carro <-
      movimento_carro(
        decisao = neural(x = recursos, w_1 = w_1, w_2 = w_2),
        sensibilidade = sensibilidade,
        coordenada_atual_carro = coordenada_atual_carro
      )
    
      # Não é preciso salvar novamente essa coordenada. O carro está sendo
      # desenhado novamente para não ser removido nas iterações de movimento do
      # buraco.
      
      if(is.na(check_colisao(sensibilidade = sensibilidade, coordenada_atual_buraco = coordenada_atual_buraco, coordenada_atual_carro = coordenada_atual_carro))){
        break
      }
    } # Fim do loop de movimento do buraco
    return(
      list(
        coordenada_atual_carro = coordenada_atual_carro,
        coordenada_atual_buraco = coordenada_atual_buraco
      )
    )
  }

IA <-  
  function(
    sensibilidade = 2L, 
    som = TRUE
  ){
    # Lendo a rede treinada
    load("rede_treinada/w_1.RData")
    load("rede_treinada/w_2.RData")
    
    movimento_aleatorio_buraco(
      sensibilidade = sensibilidade,
      w_1 = melhor_w_1,
      w_2 = melhor_w_2,
      coordenada_atual_carro
    )
  }

max_list <- function(lista){
  one_step <- function(i){
    lista[[i]]$vetor_sucessos
  }
  result <- sapply(X = 1L:length(lista), FUN = one_step)
  which.max(result[1L, ])
}