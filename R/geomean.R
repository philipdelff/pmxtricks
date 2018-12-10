##' calculate geometric  mean
##'
##' @param x vector to calculate the geometric mean of
##' @param z.rm removes zeros before calculation?

geomean <- function(x,z.rm=F) {
    if(z.rm) x <- x[x!=0]
    exp(mean(log(x)))
}

