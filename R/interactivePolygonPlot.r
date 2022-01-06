#' Function interactivePolygonPlot
#' 
#' @export
interactivePolygonPlot <- function (rver, pol, border = "blue", lim = NULL, culling = "back") 
{
    dev.new()
    if (is.null(lim)) 
        lim = c(min(tver), max(tver))
    p = function(a = F, tx = 0, ty = 0, tz = 0) {
        print(paste(tx, ";", ty, ";", tz))
        tver = transformRver(rver, ver, thetaX = tx, thetaY = ty, 
            thetaZ = tz)
        plotPolygons(rver, tver, pol, border = border, lim = lim, 
            add = a, culling = culling)
    }
    p(F)
    thetax = 0
    thetay = 0
    toDegree <- function(rad) rad * 57.2957795130823
    toRadians <- function(deg) deg/57.2957795130823
    f = function(prompt = "Move the cube with cursor keys") getGraphicsEvent(prompt = prompt, 
        onKeybd = function(x) {
            if (x == "ctrl-[") {
                print(paste("thetax=", thetax, "; thetay=", thetay))
                return("ctrl-[")
            }
            if (x == "Down") 
                thetax <<- thetax + 5
            if (x == "Left") 
                thetay <<- thetay + 5
            if (x == "Right") 
                thetay <<- thetay - 5
            if (x == "Up") 
                thetax <<- thetax - 5
            if (thetax > 360) 
                thetax <<- thetax - 360
            if (thetay > 360) 
                thetay <<- thetay - 360
            if (thetax < 0) 
                thetax <<- 360 + thetax
            if (thetay < 0) 
                thetay <<- 360 + thetay
            p(F, thetax, thetay)
            f(paste(x, "; ", thetax, ";", thetay))
            return(-1)
            NULL
        })
    f()
}