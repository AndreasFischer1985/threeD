#' Function vecDistance
#' 
#' @export
vecDistance <- function (a, b) 
sqrt(sum(sapply(1:length(a), function(i) (a[i] - b[i])^2)))
