##' Compare element names in lists
##'
##' Useful interactive tool when merging or binding objects
##' together. It lists the union of element names and indicates in
##' which of the objects they are present. It does not compare
##' contents of the elements at all.
##' 
##' @param ... objects which element names to compare
##' @param keepNames If TRUE, the original dataset names are used in reported
##'     table. If not, generic x1, x2,... are used. The latter may be preferred
##'     for readability.
##' @param debug If TRUE, browser is called to begin with.
##' @family DataWrangling
##' @export

### Todo

## sort by how many available, then where they are available

## diffOnly argument. Often you just want to see where the data.frames differ.

### End todo

compareNames <- function(...,keepNames=T,debug=F){
    ## Compares the names of the contents of lists (can be
    ## data.frames). This is useful when combining datasets to get an
    ## overview of compatibility.

    if(debug) browser()

    dots <- list(...)
    if(length(dots)<2) stop("At least two objects must be supplied")
    if(keepNames){
        names.dots <- setdiff(as.character(match.call(expand.dots=T)),as.character(match.call(expand.dots=F)))
    } else {
        names.dots <- paste0("x",seq(length(dots)))
    }
    
    cnames <- lapply(dots,function(x)sort(names(x)))

    allnms <- unique(unlist(cnames))

    mat.nms <- do.call(data.frame,lapply(cnames,function(x)ifelse(allnms%in%x,rep("x",length(allnms)),rep("",length(allnms)))))
    
    rownames(mat.nms) <- allnms

    colnames(mat.nms) <- names.dots

    
    ## browser()
    mat.nms <- mat.nms[do.call(order,mat.nms),]
    no.descripancy <- all(rowSums(mat.nms=="x")==length(dots))

    if(testEqual) return(no.descripancy)
    
    if(no.descripancy&&quietIfEqual) {
        return(invisible(mat.nms))
    }
    
    mat.nms
}

