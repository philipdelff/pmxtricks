##' Report numbers with a fixed formatting.
##' 
##' sigdigs is a wrapper that rounds whatever numeric in a data.frame
##' (because round(df) returns an error if df contains
##' non-numerics). You often want to run this function before creating
##' a csv file as output.
##'
##' @param x a numeric vector or a data.frame with some of the columns numeric
##' @param n the number of digits to round to. Only a single scalar supported.
##'
##' @examples
##' 
##' df1 <- data.frame(obs=letters[1:3],x1=rnorm(3),x2=c(1.24/1e4,1.1334e6,1.1),x3=1:3)
##' roundDF(df1,digits=3)

roundDF <- function(x,FUN=signif.c,...){

    ## do this for a numerical vector
    if (is.function(FUN)){
        roundfun <- FUN
    } else {
        stop("FUN is not a function.")
    }

    ## a switch if numeric or not
    roundfun1 <- function(x,...){
        if(is.numeric(x)) {
            return(roundfun(x,...))
        } else {
            return(x)
        }
    }
    if(is.data.frame(x)){
        x2 <- do.call(function(...)data.frame(...,stringsAsFactors=F),lapply(x,roundfun1,...))
        names(x2) <- names(x)
       
    } else {
        x2 <- roundfun1(x,...)
        
    }

    return(x2)
}
