#' Function xRotateRver
#' 
#' @export
xRotateRver <- function (rver, theta) 
{
    while (theta >= 360) theta = theta - 360
    theta[theta < 0] = 0
    theta = theta * pi/180
    ct = cos(theta)
    st = sin(theta)
    rver2 = rver
    rver2["yx"] = rver["yx"] * ct + rver["zx"] * st
    rver2["yy"] = rver["yy"] * ct + rver["zy"] * st
    rver2["yz"] = rver["yz"] * ct + rver["zz"] * st
    rver2["y0"] = rver["y0"] * ct + rver["z0"] * st
    rver2["zx"] = rver["zx"] * ct - rver["yx"] * st
    rver2["zy"] = rver["zy"] * ct - rver["yy"] * st
    rver2["zz"] = rver["zz"] * ct - rver["yz"] * st
    rver2["z0"] = rver["z0"] * ct - rver["y0"] * st
    rver2
}
