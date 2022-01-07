#' Function plotPoints
#' 
#' @export
plotPoints <- function (tver, add = F, col1 = "lightgrey", col2 = "black", 
    subset = NULL, lim = NULL) 
{
    if (is.null(lim)) {
        if (add == F) 
            plot(c(min(tver), max(tver)), c(min(tver), max(tver)), 
                type = "n", axes = F, xlab = NA, ylab = NA)
    }
    else if (add == F) 
        plot(c(lim[1], lim[2]), c(lim[1], lim[2]), type = "n", 
            axes = F, xlab = NA, ylab = NA)
    z = sapply(seq(1, length(tver), by = 3), function(i) tver[i + 
        2])
    cols = colorRampPalette(c(col2, col1))(length(levels(as.factor(z))))
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
