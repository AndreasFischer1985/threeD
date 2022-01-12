#' Function sortVer
#' 
#' @export
sortVer <- function (tver) 
{
    for (i in seq(1, length(tver), by = 3)) for (j in seq(1, 
        length(tver), by = 3)) if (tver[i + 2] > tver[j + 2]) {
        h1 = tver[i]
        h2 = tver[i + 1]
        h3 = tver[i + 2]
        tver[i] = tver[j]
        tver[i + 1] = tver[j + 1]
        tver[i + 2] = tver[j + 2]
        tver[j] = h1
        tver[j + 1] = h2
        tver[j + 2] = h3
    }
    tver
}
