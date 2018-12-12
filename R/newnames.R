##' rename data with standard evaluation



newnames <- function(data,names,overwrite=F){
    ## check that old and new are characters and equally long

    ## check that all old exist
    n.data <- names(data)
    if(!all(names$old  %in% n.data)){
        stop("These old names are missing in data:",paste(names$old[!names$old  %in% n.data],collapse=", "))
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
