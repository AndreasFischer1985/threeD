#' Function translateRver
#' 
#' @export
translateRver <- function (rver, fx, fy, fz) 
{
    rver["x0"] = rver["x0"] + fx
    rver["y0"] = rver["y0"] + fy
    rver["z0"] = rver["z0"] + fz
    rver
}
