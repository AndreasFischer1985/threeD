#' Function vecToVer
#' 
#' @export
vecToVer <- function (x, y, z) 
c(sapply(1:length(x), function(i) c(x[i], y[i], z[i])))
