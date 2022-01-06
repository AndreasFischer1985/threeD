#' Function plotPolygons
#' 
#' @export
plotPolygons <- function (rver, tver, pol, add = F, col1 = "lightgrey", col2 = "black", 
    border = "black", culling = "none", lim = NULL, fun = mean) 
{
    if (is.null(lim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(lim[1], lim[2]), c(lim[1], lim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    if (is.null(culling)) 
        culling = "none"
    pol = sortPol(pol, tver, fun)
    cols = colourPol(pol, tver, col1)
    co = 0
    for (i in seq(1, length(pol), by = 3)) {
        co = co + 1
        cp = vecCrossProduct(c(tver[pol[i + 1] * 3 - 2] - tver[pol[i + 
            0] * 3 - 2], tver[pol[i + 1] * 3 - 1] - tver[pol[i + 
            0] * 3 - 1], tver[pol[i + 1] * 3 - 0] - tver[pol[i + 
            0] * 3 - 0]), c(-tver[pol[i + 2] * 3 - 2] + tver[pol[i + 
            0] * 3 - 2], -tver[pol[i + 2] * 3 - 1] + tver[pol[i + 
            0] * 3 - 1], -tver[pol[i + 2] * 3 - 0] + tver[pol[i + 
            0] * 3 - 0]))
        if ((culling != "front" & culling != "back") | (cp %*% 
            c(0, 0, -1) >= 0 & culling == "back") | (cp %*% c(0, 
            0, -1) <= 0 & culling == "front")) 
            polygon(x = c(tver[pol[i] * 3 - 2], tver[pol[i + 
                1] * 3 - 2], tver[pol[i + 2] * 3 - 2]), y = c(tver[pol[i] * 
                3 - 1], tver[pol[i + 1] * 3 - 1], tver[pol[i + 
                2] * 3 - 1]), col = cols[co], border = border)
    }
}
