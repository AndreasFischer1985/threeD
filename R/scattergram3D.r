#' Function scattergram3D
#' 
#' @export
scattergram3D <- function (x, y, z, thetaX = 320, thetaY = 320, thetaZ = 0, col1 = "darkgrey", 
    col2 = "black", border = NA) 
{
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
    plotWireframe(rver, tver, con, add = F, subset = "back")
    plotPoints(rver, tver2, add = T, col1 = col1, col2 = col2)
    plotWireframe(rver, tver, con, add = T, subset = "front")
}
