##Funciones que usarán otras funciones más específicas

base_has_X <- function(input, X){
  return(grepl(X, input))
}