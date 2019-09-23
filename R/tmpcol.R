##' generate a name for a new data column that is not already in use.
##' @param data The dataset to find a new element name for
##' @param base The base name of the new element. A number will appended to this string that will ensure that the new element name is not already in use.
##' @param max.it Maximum number of iterations on element name.
##' @family DataWrangling
##' @export
tmpcol <- function(data,base="atmpcol999",max.it=100){
    i <- 0
    while(paste0(base,i)%in%names(data)) {
        i  <- i+1
        if(i>max.it) stop("Maximum number of iterations reached. Check if something is wrong, and maybe increase max.it.") 
    }
    colname <- paste0(base,i)
    return(colname)
}
