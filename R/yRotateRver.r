#' Function yRotateRver
#' 
#' @export
yRotateRver <- function (rver, theta) 
{
    while (theta >= 360) theta = theta - 360
    theta[theta < 0] = 0
    theta = theta * pi/180
    ct = cos(theta)
    st = sin(theta)
    rver2 = rver
    rver2["xx"] = rver["xx"] * ct + rver["zx"] * st
    rver2["xy"] = rver["xy"] * ct + rver["zy"] * st
    rver2["xz"] = rver["xz"] * ct + rver["zz"] * st
    rver2["x0"] = rver["x0"] * ct + rver["z0"] * st
    rver2["zx"] = rver["zx"] * ct - rver["xx"] * st
    rver2["zy"] = rver["zy"] * ct - rver["xy"] * st
    rver2["zz"] = rver["zz"] * ct - rver["xz"] * st
    rver2["z0"] = rver["z0"] * ct - rver["x0"] * st
    rver2
}
