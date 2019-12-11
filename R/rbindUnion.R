##' rbind data.frames by filling in NA's
##'
##' This function is an implementation of the functionality as in
##' plyr::rbind.fill for not being dependent on that package. The
##' function is no longer maintained, and it is advised to use
##' data.table::rbind(...,fill) instead.
##' 
##' @param ... data frames to stack. Must be of class data.frame.
##' @return a data.frame containing all data
##' @details Consider using data.table::rbindlist(...,fill=TRUE) instead.
##' @family DataWrangling
##' @examples
##' x <- data.frame(a=1:3)
##' y <- data.frame(b=1:3,c="h")
##' z <- data.frame(a=4:5,b=letters[4:5],d=c(NA,5))
##' pmxtricks:::rbindUnion(x,y,z)

### No longer to be exported. Use data.table::rbind(...,fill=T) instead.


rbindUnion <- function(...){

    dots <- list(...)

    ### if only a list is supplied, we expect that to be a list of data.frames.
    if(length(dots)==1&&is.list(dots[[1]])){
        dots <- dots[[1]]
    }
    
    if(!all(unlist(lapply(dots,is.data.frame)))){
        stop("All arguments must be data frames")
    }

    names.dots <- lapply(dots,names)
    names.all <- unique(unlist(names.dots))
    extend.df <- function(x){
        if(nrow(x)){
            x[names.all[!names.all%in%names(x)]] <- NA
        } else {
            x <- NULL
        }
        x
    }
    alldat <- do.call(rbind,lapply(dots,extend.df))

    return(alldat)    
}

