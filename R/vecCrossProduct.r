#' Function vecCrossProduct
#' 
#' @export
vecCrossProduct <- function (x, y) 
{
    if (is.vector(x) && is.vector(y)) {
        if (length(x) == length(y) && length(x) == 3) {
            xxy <- c(x[2] * y[3] - x[3] * y[2], x[3] * y[1] - 
                x[1] * y[3], x[1] * y[2] - x[2] * y[1])
        }
    }
    else {
        if (is.matrix(x) && is.matrix(y)) {
            if (all(dim(x) == dim(y))) {
                if (ncol(x) == 3) {
                  xxy <- cbind(x[, 2] * y[, 3] - x[, 3] * y[, 
                    2], x[, 3] * y[, 1] - x[, 1] * y[, 3], x[, 
                    1] * y[, 2] - x[, 2] * y[, 1])
                }
                else {
                  if (nrow(x) == 3) {
                    xxy <- rbind(x[2, ] * y[3, ] - x[3, ] * y[2, 
                      ], x[3, ] * y[1, ] - x[1, ] * y[3, ], x[1, 
                      ] * y[2, ] - x[2, ] * y[1, ])
                  }
                }
            }
        }
    }
    return(xxy)
}
