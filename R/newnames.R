##' rename list elements with standard evaluation
##'
##' @param data A list (which can be a data.frame)
##' @param names A list that contains elements "old" and "new" of
##'     equal length.
##' @param If names$new contains a name of an existing element in
##'     data, should it be overwritten?
##' @param skipMissing Just skip if names refers to non-existing elements?
##' @param debug Start by calling browser()?

newNames <- function(data,names,overwrite=F,skipMissing=TRUE,debug=F){
    if(debug) browser()
    ## check that old and new are characters and equally long

### if input is factors
    names$old <- as.character(names$old)
    names$new <- as.character(names$new)
    names <- data.frame(old=names$old,
                        new=names$new,
                        stringsAsFactors=F)
    
    ## check that all old exist
    n.data <- names(data)
    if(!all(names$old  %in% n.data)){
        if(skipMissing){
            tokeep <- names$old  %in% n.data
            if(sum(!toKeep)==0) return(data)
            names <- names[toKeep,]
        } else{
            stop("These old names are missing in data: ",paste(names$old[!names$old  %in% n.data],collapse=", "))
        }
    }

    ## new names must be unique
    if(any(duplicated(names$new))) stop("Don't know how to handle duplicates in new names.")
    
    ## check that all new (and not old) do not exist. If they do, overwrite must be TRUE to continue.
    if(any(names$new%in%n.data)) {
        if(overwrite){
            data <- data[,!(n.data%in%names$new&!n.data%in%names$old)]
            n.data <- names(data)
        } else {
            stop("new names already exist in data (and overwrite=F):",paste(names$new[names$new%in%n.data],collapse=", "))
        }
    }
    

    ##browser()
    
    n.data[match(names$old,n.data)] <- names$new
    names(data) <- n.data

    data
}
