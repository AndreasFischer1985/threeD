#' Function scaleRver
#' 
#' @export
scaleRver <- function (rver, fx, fy, fz) 
{
    rver["xx"] = rver["xx"] * fx
    rver["xy"] = rver["xy"] * fx
    rver["xz"] = rver["xz"] * fx
    rver["x0"] = rver["x0"] * fx
    rver["yx"] = rver["yx"] * fy
    rver["yy"] = rver["yy"] * fy
    rver["yz"] = rver["yz"] * fy
    rver["y0"] = rver["y0"] * fy
    rver["zx"] = rver["zx"] * fz
    rver["zy"] = rver["zy"] * fz
    rver["zz"] = rver["zz"] * fz
    rver["z0"] = rver["z0"] * fz
    rver
}
