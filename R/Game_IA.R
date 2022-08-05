source("funcoes_auxiliares.R")

jogando <- 
  function(
    start_carro = c(20, 15),
    sensibilidade = 2L,
    eixos = TRUE,
    som = TRUE
  ){

  if(start_carro[1L] < 6) stop("Escolhar um valor de 6<=x<=100 e 5<=y<=25")
    
  graphics.off() # Removendo sensibilidade anterior, caso exista
  plot.new()
   
  # Iniciando o background na sequência de frames
  backgroud(eixos = eixos)

  # Iniciando a sequência de frames
  sucesso <- 1L 
  repeat{
    # Escrevendo o número de sucessos até a colisão
  
    IA(
      sensibilidade = sensibilidade,
      start_carro = start_carro,
      som = som
    )
    
    text(x = 15.5, y = 27, glue("Sucesso(s) até a colisão: {sucesso}"),
         cex = 1.5, font = 2)
    
    # # Impostante para a legenda de contagem de sucessos
    polygon(
      x = c(0, 27, 27, 0),
      y = c(26, 26, 28, 28),
      col = "#009474",
      border = NA
    )
    
    if(is.na(check_colisao(sensibilidade, start_carro, som = som))){
       text(x = 15.5, y = 27, glue("Sucesso(s) até a colisão: {sucesso}"),
            cex = 1.5, font = 2)
       rasterImage(
         rip, start_carro[1L] + 5.5 - xinch(1),
         start_carro[2L] - yinch(0.8),
         start_carro[1] + 5.5,
         start_carro[2L]
       )
       break
     }
    
    text(x = 15.5, y = 27, glue("Sucesso(s) até a colisão: {sucesso}"),
         cex = 1.5, font = 2)
    
    if(som) beep(10)
    
    sucesso <- sucesso + 1L    
  }
  sucesso
}

jogando(start_carro = c(20,20), eixos = FALSE)