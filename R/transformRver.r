#' Function transformRver
#' 
#' @export
transformRver <- function (rver, ver, thetaX = NULL, thetaY = NULL, thetaZ = NULL, 
    persp = 0) 
{
    tver = ver * 0
    if (!is.null(thetaX)) 
        rver = xRotateRver(rver, thetaX)
    if (!is.null(thetaY)) 
        rver = yRotateRver(rver, thetaY)
    if (!is.null(thetaZ)) 
        rver = zRotateRver(rver, thetaZ)
    for (i in seq(length(ver), 1, by = -3) - 2) {
        tver[i + 0] = ver[i] * rver["xx"] + ver[i + 1] * rver["xy"] + 
            ver[i + 2] * rver["xz"] + rver["x0"]
        tver[i + 1] = ver[i] * rver["yx"] + ver[i + 1] * rver["yy"] + 
            ver[i + 2] * rver["yz"] + rver["y0"]
        tver[i + 2] = ver[i] * rver["zx"] + ver[i + 1] * rver["zy"] + 
            ver[i + 2] * rver["zz"] + rver["z0"]
        if (persp > 0) {
            tver[i + 0] = tver[i + 0] * (1 - tver[i + 2]/persp)
            tver[i + 1] = tver[i + 1] * (1 - tver[i + 2]/persp)
        }
    }
    return(tver)
}
