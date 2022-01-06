#' Function vecLength
#' 
#' @export
vecLength <- function (...) 
sqrt(sum(sapply(..., function(x) x^2)))
