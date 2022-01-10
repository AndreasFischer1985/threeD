#' Function scattergram3D
#' 
#' @export
scattergram3D <- function (x = NULL, y = NULL, z = NULL, thetaX = 320, thetaY = 320, 
    thetaZ = 0, col = c("darkgrey", "black"), add.polygon = F, 
    border = NA, main = "3D Scattergram") 
{
    if (is.null(x) & is.null(y) & is.null(z)) {
        x = rnorm(100)
        y = rnorm(100)
        z = rnorm(100)
    }
    cube = form("cube")
    ver = cube$ver
    con = cube$con
    pol = cube$pol
    rver = unitRver()
    tver = transformRver(rver, ver, thetaX, thetaY, thetaZ)
    max = max(c(abs(x), abs(y), abs(z)), na.rm = T)
    x = x/max
    y = y/max
    z = z/max
    poi = vecToVer(x, y, z)
    tver2 = transformRver(rver, poi, thetaX, thetaY, thetaZ)
    if (add.polygon) 
        plotPolygons(tver, pol, culling = "front", border = border, 
            add = F, col = col)
    if (is.null(col)) 
        col = "black"
    if (length(col) == 1) 
        col = rep(col, 2)
    plotWireframe(tver, con, add = F, subset = "back")
    plotPoints(tver2, add = T, col = col)
    plotWireframe(tver, con, add = T, subset = "front")
    if (!is.null(main)) 
        title(main)
    invisible(list(ver = ver, tver = tver2))
}
