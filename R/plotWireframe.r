#' Function plotWireframe
#' 
#' @export
plotWireframe <- function (tver = form("cube")$ver, con = form("cube")$con, add = F, 
    col = c("lightgrey", "black"), subset = NULL, xlim = NULL, 
    ylim = NULL, fun = mean) 
{
    if (is.null(xlim) | is.null(ylim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(xlim[1], xlim[2]), c(ylim[1], ylim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    con = sortCon(con, tver, fun)
    z = sapply(seq(1, length(con), by = 2), function(i) (tver[con[i] * 
        3] + tver[con[i + 1] * 3])/2)
    if (is.null(col)) 
        col = "black"
    if (length(col) == 1) 
        col = rep(col, 2)
    cols = colorRampPalette(c(col[2], col[1]))(length(levels(as.factor(z))))
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
