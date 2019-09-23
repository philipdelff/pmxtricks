##' rbind data.frames by using only common columns
##'
##' @param ... data frames to stack. Must be of class data.frame.
##' @return a data.frame containing all data
##' @family DataWrangling
##' @export  
##' @examples
##' x <- data.frame(a=1:3)
##' y <- data.frame(b=1:3,c="h")
##' z <- data.frame(a=4:5,b=letters[4:5],d=c(NA,5))
##' rbindIntersect(x,y,z)

rbindIntersect <- function(...){

    dots <- list(...)

    ### if only a list is supplied, we expect that to be a list of data.frames.
    if(length(dots)==1&&is.list(dots[[1]])){
        dots <- dots[[1]]
    }
    
    if(!all(unlist(lapply(dots,is.data.frame)))){
        stop("All arguments must be data frames")
    }

    names.dots <- lapply(dots,names)
    names.all <- Reduce(intersect,names.dots)
    reduce.df <- function(x){
        x[,names.all]
    }
     
    alldat <- do.call(rbind,lapply(dots,reduce.df))

    return(alldat)
}

