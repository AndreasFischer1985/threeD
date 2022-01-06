#' Function toStringRver
#' 
#' @export
toStringRver <- function (rver) 
{
    x = paste0("[", rver["x0"], ",", rver["xx"], ",", rver["xy"], 
        ",", rver["xz"], ";\n", rver["y0"], ",", rver["yx"], 
        ",", rver["yy"], ",", rver["yz"], ";\n", rver["z0"], 
        ",", rver["zx"], ",", rver["zy"], ",", rver["zz"], "]")
    message(x)
    return(x)
}
