#' Function sortPol
#' 
#' @export
sortPol <- function (pol, tver, fun = mean) 
{
    if (is.null(fun)) 
        fun = mean
    for (i in seq(1, length(pol), by = 3)) for (j in seq(1, length(pol), 
        by = 3)) if (fun(c(tver[pol[i] * 3], tver[pol[i + 1] * 
        3], tver[pol[i + 2] * 3])) > fun(c(tver[pol[j] * 3], 
        tver[pol[j + 1] * 3], tver[pol[j + 2] * 3]))) {
        h1 = pol[i]
        h2 = pol[i + 1]
        h3 = pol[i + 2]
        pol[i] = pol[j]
        pol[i + 1] = pol[j + 1]
        pol[i + 2] = pol[j + 2]
        pol[j] = h1
        pol[j + 1] = h2
        pol[j + 2] = h3
    }
    return(pol)
}
