#' Function plotWireframe
#' 
#' @export
plotWireframe <- function (rver, tver, con, add = F, col1 = "lightgrey", col2 = "black", 
    subset = NULL, lim = NULL, fun = mean) 
{
    if (is.null(lim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(lim[1], lim[2]), c(lim[1], lim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    con = sortCon(con, tver, fun)
    z = sapply(seq(1, length(con), by = 2), function(i) (tver[con[i] * 
        3] + tver[con[i + 1] * 3])/2)
    cols = colorRampPalette(c(col2, col1))(length(levels(as.factor(z))))
    if (!is.null(subset)) {
        if (subset == "front") 
            cols[z <= 0] = NA
        if (subset == "back") 
            cols[z > 0] = NA
    }
    co = 0
    for (i in seq(1, length(con), by = 2)) {
        co = co + 1
        segments(tver[con[i] * 3 - 2], tver[con[i] * 3 - 1], 
            tver[con[i + 1] * 3 - 2], tver[con[i + 1] * 3 - 1], 
            col = cols[rank(z, ties.method = "min")[co]])
    }
}
