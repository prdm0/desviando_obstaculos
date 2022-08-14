# Bibliotecas
library(magick)
library(beepr)
library(glue)
library(pbapply)
library(furrr)
library(pbmcapply)

# Limopando memória
rm(list = ls(all = TRUE))

# Chamando arquivos
source("R/rede_aviao.R")
source("R/genetic_aviao.R")

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

raster_muro <- magick::image_read(path = "imagens/muro.png")

coordenada_atual_carro <- c(20, 15)
coordenada_atual_buraco <- double(length = 2L)
distancia_carro_buraco <- double(length = 1L)

apagar <- function(coord_obj, carro = FALSE){
  x <- coord_obj[1L]
  y <- coord_obj[2L]
  
  x_vec <- c(x - 3.5, x + 0.1, x + 0.1, x - 3.5)
  y_vec <- c(y - 2.25, y - 2.25, y + 0.1, y + 0.1)
  
  if(carro){
    x_vec <- c(x - 8, x + 0.1, x + 0.1, x - 8)
    y_vec <- c(y - 3, y - 3, y, y)
  }
  
  polygon(
    x = x_vec,
    y = y_vec,
    col = "#4d4d4d",
    border = NA
  )
}

# Desenha o background do jogo
backgroud <- function(eixos = FALSE) {
  
  graphics.off() # Removendo sensibilidade anterior, caso exista
  
  x11(
    title = "DESVIE DOS OBJETOS",
    width = 20L,
    height = 10L,
    bg = "#009474"
  )
  
  par(mar = c(1, 0, 7, 0)) # c(inferior, esquerda, superior, direita)
  
  plot.new()
  
  plot.window(xlim = c(1, 100), ylim = c(1, 30))
  
  title("Rede Neural Treinada com Algoritmo Genético", cex.main = 2.5)
  mtext("O carro usará a Inteligência Artificial (IA) para tomar a decisão de desviar dos obstáculos aleatórios", cex = 1.5)
  # Polígono: desenho do asfalto
  polygon(
    x = c(-10, 120, 120, -10),
    y = c(4.93, 4.93, 25.05, 25.05),
    col = "#4d4d4d",
    border = NA
  )
  
  abline(h = 5L - 0.24, lwd = 10L, col = "#E79B4D")
  abline(h = 25L +  0.24, lwd = 10L, col = "#E79B4D")
  
  if(eixos) {
    axis(1L)
    axis(2L)
  }
}

desenhar_muro <- function(x, apagar = FALSE){
  
  y <- 5L
  
  if(apagar){
    polygon(
      x = c(x - 1.4, x + 6, x + 6, x - 1.4),
      y = c(5, 5, 25.05, 25.05),
      col = "#4d4d4d",
      border = NA
    )
  }else{
    rasterImage(raster_muro, x + xinch(1), y + yinch(5.646), x, y) 
  }
}

desenhar <- 
  function(
    coord_obj,
    add_carro = TRUE,
    controlar_local = TRUE,
    b_cinza = FALSE,
    graphic = TRUE
  ){
    
    x <- coord_obj[1L]
    y <- coord_obj[2L]
    
    # A variável controlar_local força a plotagem do objeto dentro
    # dos limites do asfalto.
    if(add_carro && controlar_local && x >= 100) x <- 100
    if(add_carro && controlar_local && x <= 7) x <- 12.35
    if(add_carro && controlar_local && y <= 7) y <- 7.5
    if(add_carro && controlar_local && y >= 25) y <- 24.8
    
    if(controlar_local && x >= 100) x <- 100
    if(controlar_local && x <= 7) x <- 12.35
    if(controlar_local && y <= 7) y <- 7.5
    if(controlar_local && y >= 25) y <- 24.8
    
    if(graphic){
      if(add_carro){
        rasterImage(raster_carro, x - xinch(1.3), y - yinch(0.8), x, y)
      }else{
        if(b_cinza){
          apagar(coord_obj = c(x, y))
        }else{
          rasterImage(raster_buraco, x - xinch(0.6), y - yinch(0.6), x, y)
        }
      } 
    }
    list(coord_obj = coord_obj, x = coord_obj[1L], y = coord_obj[2L])
  }

check_colisao <- function(sensibilidade = 2L, start_carro = c(20, 15), som = TRUE, graphic = TRUE) {
  
  if(coordenada_atual_carro[2L] < 5 || coordenada_atual_carro[2L] > 25) return(NA)
  
  if(abs(coordenada_atual_carro[2L] - coordenada_atual_buraco[2L]) <= sensibilidade){
    toque_y <- TRUE  
  } else {
    toque_y <- FALSE
  }
  
  if(toque_y && (coordenada_atual_buraco[1L] - start_carro[1L]) < 0.03){
    if(graphic) desenhar(coord_obj = coordenada_atual_carro)
    if(som) beep(8)
    return(NA)
  } 
  return(-1L)
}

movimento_carro <- function(decisao, sensibilidade = 2L, graphic = TRUE){
  
  if(graphic) apagar(coord_obj = coordenada_atual_carro, carro = TRUE)
  
  switch (
    decisao,
    cima = {
      coordenada_atual_carro[2L] <<- coordenada_atual_carro[2L] + sensibilidade
    },
    baixo = {
      coordenada_atual_carro[2L] <<- coordenada_atual_carro[2L] - sensibilidade
    },
    parado = {
      coordenada_atual_carro[2L] <<- coordenada_atual_carro[2L] + 0L
    }
  )
  
  if(coordenada_atual_carro[2L] > 25) coordenada_atual_carro[2L] <<- 25
  if(coordenada_atual_carro[2L] < 8) coordenada_atual_carro[2L] <<- 8
  
  if(graphic){
    desenhar(coord_obj = coordenada_atual_carro, add_carro = TRUE, graphic = graphic)
  }
}

movimento_aleatorio_buraco <- 
  function(sensibilidade, 
           w_1, 
           w_2,
           fun = "gaussian",
           graphic = TRUE,
           som = TRUE
           ){
    
    # Coordenadas aleatóra do buraco
    coordenada_atual_buraco <<- 
      c(
        sample(x = (coordenada_atual_carro[1L] + 5L):100L, size = 1L),
        sample(x = 7:25L, size = 1L)
      )
    
    desenhar(coord_obj = coordenada_atual_carro, add_carro = TRUE, graphic = graphic) # desenho do carro

    # Não poderá ser feita por funcional, uma vez que contém a aleatoriedade 
    # da colisão, que poderá eventualmente ocorrer.
    
    for(x_buraco in coordenada_atual_buraco[1L]:coordenada_atual_carro[1L]){

      desenhar(
        coord_obj = c(coordenada_atual_buraco[1L], coordenada_atual_buraco[2L]),
        add_carro = FALSE,
        b_cinza = TRUE,
        graphic = graphic
      )

      # Desenhando um buraco e salvando na variável global a posição
      # atual do buraco.
      coordenada_atual_buraco <<- c(x_buraco, coordenada_atual_buraco[2L])

      desenhar(
        coord_obj = c(x_buraco, coordenada_atual_buraco[2L]),
        add_carro = FALSE,
        graphic = graphic
      )

      recursos <-
        c(
          1L,
          coordenada_atual_carro[1L],
          coordenada_atual_carro[2L],
          coordenada_atual_buraco[1L],
          coordenada_atual_buraco[2L],
          dist(rbind(coordenada_atual_buraco, coordenada_atual_carro))
        )

      movimento_carro(
        decisao = neural(x = recursos, w_1 = w_1, w_2 = w_2, fun = fun),
        sensibilidade = sensibilidade,
        graphic = graphic
      )

      # Não é preciso salvar novamente essa coordenada. O carro está sendo
      # desenhado novamente para não ser removido nas iterações de movimento do
      # buraco.

      if(graphic)
        polygon(
          x = c(0,coordenada_atual_carro[1L] + 0.1 , coordenada_atual_carro[1L] + 0.1, 0),
          y = c(5,5,25,25),
          col = "#4d4d4d",
          border = NA
        )

      desenhar(coord_obj = coordenada_atual_carro, add_carro = TRUE, graphic = graphic) # desenho do carro

      if(is.na(check_colisao(sensibilidade = sensibilidade, start_carro = coordenada_atual_carro, som = som, graphic = graphic))){
        break
      }
    } # Fim do loop de movimento do buraco
  }

IA <-  
  function(
    sensibilidade = 2L, 
    start_carro = c(20, 15),
    fun = "gaussian",
    som = TRUE
  ){
    # Lendo a rede treinada
    load("rede_treinada/w_1.RData")
    load("rede_treinada/w_2.RData")
    
    movimento_aleatorio_buraco(
      sensibilidade = sensibilidade,
      w_1 = melhor_w_1,
      w_2 = melhor_w_2,
      fun = fun,
      graphic = TRUE,
      som = som
    )
}