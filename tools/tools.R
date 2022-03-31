
#' Scale data between 0 and 1
#'
#' @param data A data frame
#' 
#' @return a data frame with all the attributes scaled between 0 and 1
#' 
#' @examples
#' normalize(data)
normalize <- function(data){
  return(sapply(data,  function(x){(x-min(x))/(max(x)-min(x))}))
}
