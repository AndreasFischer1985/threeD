#' Function plotPoints
#' 
#' @export
plotPoints <- function (tver, add = F, col = c("lightgrey", "black"), subset = NULL, 
    xlim = NULL, ylim = NULL) 
{
    if (is.null(xlim) | is.null(ylim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(xlim[1], xlim[2]), c(ylim[1], ylim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    z = sapply(seq(1, length(tver), by = 3), function(i) tver[i + 
        2])
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
    for (i in seq(1, length(tver), by = 3)) {
        co = co + 1
        points(tver[i + 0], tver[i + 1], pch = 16, col = cols[rank(z, 
            ties.method = "min")[co]])
    }
}
