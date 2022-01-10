#' Function plotPolygons
#' 
#' @export
plotPolygons <- function (tver = form("cube")$ver, pol = form("cube")$pol, add = F, 
    col = "lightgrey", transparency = 0, border = "black", culling = "none", 
    xlim = NULL, ylim = NULL, fun = mean) 
{
    if (is.null(xlim) | is.null(ylim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(xlim[1], xlim[2]), c(ylim[1], ylim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    if (is.null(culling)) 
        culling = "none"
    pol = sortPol(pol, tver, fun)
    cols = colourPol(pol, tver, col, transparency = transparency)
    borders = NULL
    if (is.null(border)) 
        borders = cols
    if (length(borders) != length(cols)) 
        borders = rep(border[1], length(cols))
    else borders = cols
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
                2] * 3 - 1]), col = cols[co], border = borders[co])
    }
}
