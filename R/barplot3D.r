#' Function barplot3D
#' 
#' @export
barplot3D <- function (x, col = NULL, transparency = 0.2, xlim = NULL, ylim = NULL, 
    main = "3D Barplot", thetaX = 10, thetaY = 10, thetaZ = 0, 
    add.numbers = F, txt.srt = 0, txt.pos = 3) 
{
    x = cbind(x)
    bar = list(ver = c(-1, 0, -1, +1, 0, -1, +1, 0, +1, -1, 0, 
        +1, -1, +2, -1, +1, +2, -1, +1, +2, +1, -1, +2, +1), 
        pol = c(1, 3, 2, 1, 4, 3, 5, 6, 7, 5, 7, 8, 1, 2, 5, 
            2, 6, 5, 2, 3, 7, 2, 7, 6, 1, 8, 4, 1, 5, 8, 3, 4, 
            8, 3, 8, 7), con = c(1, 2, 2, 3, 3, 4, 1, 4, 5, 6, 
            6, 7, 7, 8, 5, 8, 1, 5, 2, 6, 3, 7, 4, 8))
    ver = bar$ver
    con = bar$con
    pol = bar$pol
    rver = unitRver()
    tver = transformRver(rver, ver, thetaX = thetaX, thetaY = thetaY, 
        thetaZ = thetaZ)
    ver2 = ver
    ver2[seq(1, length(ver), by = 3) + 1] = ver[seq(1, length(ver), 
        by = 3) + 1] * max(x)
    ver2[seq(1, length(ver), by = 3) + 2] = ver[seq(1, length(ver), 
        by = 3) + 2] + (dim(x)[2] - 1) * 3
    ver2[seq(1, length(ver), by = 3) + 0] = ver[seq(1, length(ver), 
        by = 3) + 0] + (1 - 1) * 3
    ver3 = ver
    ver3[seq(1, length(ver), by = 3) + 1] = ver[seq(1, length(ver), 
        by = 3) + 1] * 0
    ver3[seq(1, length(ver), by = 3) + 2] = ver[seq(1, length(ver), 
        by = 3) + 2] + (1 - 1) * 3
    ver3[seq(1, length(ver), by = 3) + 0] = ver[seq(1, length(ver), 
        by = 3) + 0] + (dim(x)[1] - 1) * 3
    tver2 = transformRver(rver, ver2, thetaX = thetaX, thetaY = thetaY, 
        thetaZ = thetaZ)
    tver3 = transformRver(rver, ver3, thetaX = thetaX, thetaY = thetaY, 
        thetaZ = thetaZ)
    if (is.null(xlim)) 
        xlim = c(min(tver2[seq(1, length(tver2), by = 3) + 0]), 
            max(tver2[seq(1, length(tver2), by = 3) + 0] + (dim(x)[1] - 
                1) * 3))
    d1 = abs(min(tver2[seq(1, length(tver2), by = 3) + 1]) - 
        min(tver3[seq(1, length(tver3), by = 3) + 1]))
    d2 = (sort(tver2[seq(1, length(tver2), by = 3) + 1], decreasing = T)[1:2])
    d2 = d2[1] - d2[2]
    if (is.null(ylim)) 
        ylim = c(min(tver2[seq(1, length(tver2), by = 3) + 1]) - 
            d1 * dim(x)[2], max(tver2[seq(1, length(tver2), by = 3) + 
            1]) + d2 * dim(x)[1])
    plotPolygons(tver, pol, col = NA, transparency = transparency, 
        border = NA, xlim = xlim, ylim = ylim, add = F, culling = "back")
    if (is.null(col)) 
        col = rainbow(dim(x)[1])
    k = 1
    x1 = numeric(0)
    y1 = numeric(0)
    v1 = numeric(0)
    for (i in 1:dim(x)[1]) for (j in dim(x)[2]:1) {
        ver2 = ver
        ver2[seq(1, length(ver), by = 3) + 1] = ver[seq(1, length(ver), 
            by = 3) + 1] * x[i, j]
        ver2[seq(1, length(ver), by = 3) + 2] = ver[seq(1, length(ver), 
            by = 3) + 2] + (j - 1) * 3
        ver2[seq(1, length(ver), by = 3) + 0] = ver[seq(1, length(ver), 
            by = 3) + 0] + (i - 1) * 3
        tver2 = transformRver(rver, ver2, thetaX = thetaX, thetaY = thetaY, 
            thetaZ = thetaZ)
        plotPolygons(tver2, pol, col = col[i], transparency = transparency, 
            border = NA, xlim = xlim, ylim = ylim, add = T, culling = "back")
        k = k + 1
        x1 = c(x1, mean(tver2[seq(1, length(tver), by = 3) + 
            0][order(tver2[seq(1, length(tver), by = 3) + 1], 
            decreasing = T)[1:2]]))
        y1 = c(y1, max(tver2[seq(1, length(tver), by = 3) + 1]))
        v1 = c(v1, x[i, j])
    }
    if (!is.null(main)) 
        title(main)
    if (add.numbers == T) 
        text(x1, y1, v1, srt = txt.srt, pos = txt.pos, xpd = T)
    return(list(x = x1, y = y1, value = v1))
}
