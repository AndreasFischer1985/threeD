#' Function colourPol
#' 
#' @export
colourPol <- function (pol, tver, col1 = "lightgrey", returnDegree = F) 
{
    d = numeric(0)
    toDegree <- function(rad) rad * 57.2957795130823
    for (i in seq(1, length(pol), by = 3)) {
        x1 = tver[pol[i] * 3 - 2]
        y1 = tver[pol[i] * 3 - 1]
        z1 = tver[pol[i] * 3 - 0]
        x2 = tver[pol[i + 1] * 3 - 2]
        y2 = tver[pol[i + 1] * 3 - 1]
        z2 = tver[pol[i + 1] * 3 - 0]
        x3 = tver[pol[i + 2] * 3 - 2]
        y3 = tver[pol[i + 2] * 3 - 1]
        z3 = tver[pol[i + 2] * 3 - 0]
        v1 = (y1 - y2) * (z3 - z2) - (z1 - z2) * (y3 - y2)
        v2 = (z1 - z2) * (x3 - x2) - (x1 - x2) * (z3 - z2)
        v3 = (x1 - x2) * (y3 - y2) - (y1 - y2) * (x3 - x2)
        d = c(d, asin(sqrt((v1 * v1 + v2 * v2)/(v1 * v1 + v2 * 
            v2 + v3 * v3))))
    }
    d = toDegree(d)
    if (returnDegree) 
        return(d)
    d = round(d)
    cols = unlist(lapply(1:length(d), function(i) {
        apply(col2rgb(col1), 2, function(x) rgb(max(0, min(1, 
            (x[1] - d[i])/255)), max(0, min(1, (x[2] - d[i])/255)), 
            max(0, min(1, (x[3] - d[i])/255))))
    }))
    cols
}
