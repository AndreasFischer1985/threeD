#' Function surfaceplot3D
#' 
#' @export
surfaceplot3D <- function (x = -10:10, y = -10:10, fun = NULL, dim = c(10, 10), 
    border = NA, col = rgb(0, 0, 1), transparency = 0.1, thetaX = 10, 
    thetaY = 60, thetaZ = 10, culling = "none", add.wireframe = T, 
    add.polygons = T, ...) 
{
    if (is.null(fun)) 
        fun = function(x, y) x * y
    x = rep(seq(min(x), max(x), length.out = dim[1]), times = dim[2])
    y = rep(seq(min(y), max(y), length.out = dim[2]), each = dim[2])
    z = fun(x, y)
    ver = vecToVer(x, z, y)
    pol = numeric(0)
    con = numeric(0)
    for (j in 1:(dim[2] - 1)) {
        for (i in 1:(dim[1] - 1)) {
            pol = c(pol, (dim[1] * (j - 1)) + i + 0, (dim[1] * 
                (j - 1)) + i + dim[1] + 0, (dim[1] * (j - 1)) + 
                i + dim[1] + 1, (dim[1] * (j - 1)) + i + 0, (dim[1] * 
                (j - 1)) + i + dim[1] + 1, (dim[1] * (j - 1)) + 
                i + 1)
            con = c(con, (dim[1] * (j - 1)) + i, (dim[1] * (j - 
                1)) + i + 1, (dim[1] * (j - 1)) + i + 1, (dim[1] * 
                (j - 1)) + i + dim[1] + 1, (dim[1] * (j - 1)) + 
                i + dim[1] + 1, (dim[1] * (j - 1)) + i + dim[1] + 
                0, (dim[1] * (j - 1)) + i + dim[1] + 0, (dim[1] * 
                (j - 1)) + i)
        }
    }
    ver[seq(1, length(ver), by = 3) + 0] = (ver[seq(1, length(ver), 
        by = 3) + 0])/(max(ver[seq(1, length(ver), by = 3) + 
        0]))
    ver[seq(1, length(ver), by = 3) + 1] = (ver[seq(1, length(ver), 
        by = 3) + 1])/(max(ver[seq(1, length(ver), by = 3) + 
        1]))
    ver[seq(1, length(ver), by = 3) + 2] = (ver[seq(1, length(ver), 
        by = 3) + 2])/(max(ver[seq(1, length(ver), by = 3) + 
        2]))
    rver = unitRver()
    cube = form("cube")
    tver = transformRver(rver, ver, thetaX = thetaX, thetaY = thetaY, 
        thetaZ = thetaZ)
    tver2 = transformRver(rver, cube$ver, thetaX = thetaX, thetaY = thetaY, 
        thetaZ = thetaZ)
    plotWireframe(tver2, cube$con, add = F, subset = "back")
    if (add.polygons) 
        plotPolygons(tver, pol, border = border, add = T, col = col, 
            culling = culling, transparency = transparency, ...)
    if (add.wireframe) 
        plotWireframe(tver, con, add = T, col = c(col, "black"))
    plotWireframe(tver2, form("cube")$con, add = T, subset = "front")
    invisible(list(ver = ver, pol = pol, con = con))
}
