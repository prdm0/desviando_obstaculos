# Funções de ativação
relu <- function(x, binary = FALSE){
  if(binary){
    result <-ifelse(x > 0, 1, 0)
  } else {
    result <- ifelse(x > 0, x, 0)
  }
  return(result)
}
  
softmax <- function(x)
  exp(x)/sum(exp(x))

normalizar <- function(x)
  (x - mean(x))/sd(x)

forward <- function(x, w, fun = "softmax", norm = TRUE, binary = TRUE){

  stopifnot(length(x) == dim(w)[1L])
  
  if(norm){
    result <- normalizar(x) %*% w
  } else {
    result <- x %*% w  
  }
  
  if(fun != "softmax"){
    return(eval(call(fun, result, binary = binary)))
  }else{
    return(eval(call(fun, result)))
  }
}

neural <- function(
    x,
    w_1 = NULL,
    w_2 = NULL,
    n_saida = 3L,
    norm = TRUE,
    classes = c("parado", "cima", "baixo")
    ){
  
  stopifnot(length(classes) == n_saida)
  
  n_entrada <- length(x)

  # Inicializando w_1 e w_2
  if(is.null(w_1) || is.null(w_2)) {
    w_1 <- matrix(
      data = rnorm(n = n_entrada * n_entrada),
      nrow = n_entrada,
      ncol = n_entrada
    )
    
    w_2 <- matrix(
      data = rnorm(n = n_saida * n_entrada),
      nrow = n_entrada,
      ncol = n_saida
    ) 
  } 

  x_1 <- forward(x, w_1, fun = "relu", norm = norm, binary = TRUE)
  probs <- forward(x_1, w_2, fun = "softmax", norm = FALSE)
  
  return(classes[which.max(probs)])
}