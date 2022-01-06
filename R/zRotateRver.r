#' Function zRotateRver
#' 
#' @export
zRotateRver <- function (rver, theta) 
{
    while (theta >= 360) theta = theta - 360
    theta[theta < 0] = 0
    theta = theta * pi/180
    ct = cos(theta)
    st = sin(theta)
    rver2 = rver
    rver2["yx"] = rver["yx"] * ct + rver["xx"] * st
    rver2["yy"] = rver["yy"] * ct + rver["xy"] * st
    rver2["yz"] = rver["yz"] * ct + rver["xz"] * st
    rver2["y0"] = rver["y0"] * ct + rver["x0"] * st
    rver2["xx"] = rver["xx"] * ct - rver["yx"] * st
    rver2["xy"] = rver["xy"] * ct - rver["yy"] * st
    rver2["xz"] = rver["xz"] * ct - rver["yz"] * st
    rver2["x0"] = rver["x0"] * ct - rver["y0"] * st
    rver2
}
