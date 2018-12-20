cat("signif.c\n")
##' round to fixed number of significant digits
##'
##' @param x a numeric vector.
##' @param digits number of significant digits to round to.
##' @param add pad with zeros where digits>nchar(x[i]). Only TRUE
##'     supported.
##' @param debug start by calling browser.
##' 
##' @examples
##' x <- c(1.24e-4,1.1334e6,1.1,22.00000,10.00,1)
##' signif(x,3)
##' as.character(signif(x,3))
##' signif.c(x,3)
##' signif.c(x,3,add=T)
##' signif.c(c(.2,11.84),2)


signif.c <- function(x,digits,add=T,debug=F) {
    if(debug) browser()
    
    ## check that x is a numeric vector
    stopifnot(is.vector(x)&&is.numeric(x))
    stopifnot(is.numeric(digits)&&length(digits)==1&&(digits%/%1)==digits)
    if(!add){stop("only add=TRUE supported.")}
    
    rounded1 <- signif(x,digits)
    rounded2 <- sub("\\.0*$","",as.character(rounded1))

    rounded3 <- rounded2
    if(add){


### pad with zeros where digits>nchar(x[i]) 
        padfun <- function(y){
            paste0(y,
                   ## if not containing a '.', add one before padding
                   ifelse(grepl("\\.",y)||nchar(y)>=digits,"","."),
                   paste(rep(0,max(0,digits-nchar(sub("^0","",sub("\\.","",y))))),collapse="")
                   )
        }

        rounded3 <- unlist(lapply(rounded3,padfun))
    }
    
    rounded3
}

