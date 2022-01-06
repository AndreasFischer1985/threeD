#' Function readOBJ
#' 
#' @export
readOBJ <- function (file) 
{
    data = readLines(file, warn = F)
    f = grep("^f ", data, value = T)
    v = grep("^v ", data, value = T)
    l = grep("^l ", data, value = T)
    return(list(ver = as.numeric(grep("[^v]", unlist(strsplit(v, 
        "[ \n\r]+")), value = T)), pol = as.numeric(gsub("/.*", 
        "", grep("[^f]", unlist(strsplit(f, "[ \n\r]+")), value = T))), 
        con = unlist(lapply(strsplit(l, "[ \n\r]+"), function(x) as.numeric(rbind(x[-length(x)], 
            x[-1])[, -1])))))
}
