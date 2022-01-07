#' Function barplot3D
#' 
#' @export
barplot3D <- function (x, col = NULL, main = "3D Barplot") 
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
    tver = transformRver(rver, ver, thetaX = 10, thetaY = 10, 
        thetaZ = 10)
    ver2 = ver
    ver2[seq(1, length(ver), by = 3) + 1] = ver[seq(1, length(ver), 
        by = 3) + 1] * max(x)
    ver2[seq(1, length(ver), by = 3) + 2] = ver[seq(1, length(ver), 
        by = 3) + 2] + (dim(x)[2] - 1) * 3
    ver2[seq(1, length(ver), by = 3) + 0] = ver[seq(1, length(ver), 
        by = 3) + 0] + (dim(x)[1] - 1) * 3
    tver2 = transformRver(rver, ver2, thetaX = 10, thetaY = 10, 
        thetaZ = 10)
    l1 = min(c(tver2[seq(1, length(tver2), by = 3) + 0], tver2[seq(1, 
        length(tver2), by = 3) + 1]))
    l2 = max(c(tver2[seq(1, length(tver2), by = 3) + 0], tver2[seq(1, 
        length(tver2), by = 3) + 1]))
    plotPolygons(tver, pol, col1 = NA, border = NA, lim = c(-l1, 
        l2), add = F, culling = "back")
    if (is.null(col)) 
        col = rainbow(dim(x)[1])
    k = 1
    for (i in 1:dim(x)[1]) for (j in dim(x)[2]:1) {
        ver2 = ver
        ver2[seq(1, length(ver), by = 3) + 1] = ver[seq(1, length(ver), 
            by = 3) + 1] * x[i, j]
        ver2[seq(1, length(ver), by = 3) + 2] = ver[seq(1, length(ver), 
            by = 3) + 2] + (j - 1) * 3
        ver2[seq(1, length(ver), by = 3) + 0] = ver[seq(1, length(ver), 
            by = 3) + 0] + (i - 1) * 3
        tver2 = transformRver(rver, ver2, thetaX = 10, thetaY = 10, 
            thetaZ = 10)
        plotPolygons(tver2, pol, col1 = col[i], border = NA, 
            lim = c(l1, l2), add = T, culling = "back")
        k = k + 1
    }
    if (!is.null(main)) 
        title(main)
}
