#' Function polArea
#' 
#' @export
polArea <- function (a, b, c) 
(a[1] * b[2] - a[2] * b[1] + b[1] * c[2] - b[2] * c[1] + b[1] * 
    a[2] - b[2] * a[1])/2
