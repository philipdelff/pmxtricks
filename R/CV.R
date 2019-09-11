##' Calculate coefficient of variation
##'
##' @param x The data
##' @param log If TRUE, the geometric coefficient of variation is
##'     calculated. This is sqrt(exp(var(log(x))-1).


### for a Nonmem Omega parameter, do
## CV=sqrt(exp(Omega)-1)

CV <- function(x,log=F) {
    if(log){
        cv <- sqrt(exp(var(log(x)))-1)
    } else {
        cv <- sd(x)/mean(x)
    }
    cv
}
