#' Function unitRver
#' 
#' @export
unitRver <- function () 
{
    rver = numeric(0)
    rver["xx"] = 1
    rver["xy"] = 0
    rver["xz"] = 0
    rver["x0"] = 0
    rver["yx"] = 0
    rver["yy"] = 1
    rver["yz"] = 0
    rver["y0"] = 0
    rver["zx"] = 0
    rver["zy"] = 0
    rver["zz"] = 1
    rver["z0"] = 0
    rver
}
