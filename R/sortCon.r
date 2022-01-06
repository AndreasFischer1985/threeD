#' Function sortCon
#' 
#' @export
sortCon <- function (con, tver, fun = mean) 
{
    if (is.null(fun)) 
        fun = mean
    for (i in seq(1, length(con), by = 2)) for (j in seq(1, length(con), 
        by = 2)) if (fun(c(tver[con[i] * 3], tver[con[i + 1] * 
        3])) > fun(c(tver[con[j] * 3], tver[con[j + 1] * 3]))) {
        h1 = con[i]
        h2 = con[i + 1]
        con[i] = con[j]
        con[i + 1] = con[j + 1]
        con[j] = h1
        con[j + 1] = h2
    }
    con
}
