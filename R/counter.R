#' Simple count function
#' @param x A vector
#' @return a numeric
#' @export

counter <- function(x){
  x <- as.vector(x)
  acc <- 0
  for (i in 1:length(x)){
    if (is.na(x[i]) == FALSE){
      acc <- acc + 1
    }
  }
  return(acc)
}
