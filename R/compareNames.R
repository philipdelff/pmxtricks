

### Todo

## sort by how many available, then where they are available

## diffOnly argument. Often you just want to see where the data.frames differ.

### End todo

compareNames <- function(...,debug=F){
    ## Compares the names of the contents of lists (can be
    ## data.frames). This is useful when combining datasets to get an
    ## overview of compatibility.

    if(debug) browser()

    dots <- list(...)
    if(length(dots)<2) stop("At least two objects must be supplied")
    names.dots <- setdiff(as.character(match.call(expand.dots=T)),as.character(match.call(expand.dots=F)))
    
    cnames <- lapply(dots,function(x)sort(names(x)))

    allnms <- unique(unlist(cnames))

    mat.nms <- do.call(data.frame,lapply(cnames,function(x)ifelse(allnms%in%x,rep("x",length(allnms)),rep("",length(allnms)))))
    
    rownames(mat.nms) <- allnms
    ## arrange not easy to use here. do with order instead.
    ##    mat.nms <- arrange(cbind(Var=allnms,mat.nms),x1,x2,Var)
    ##     mat.nms <- cbind(Var=allnms,mat.nms)
    ## colnames(mat.nms) <- c("Var",names.dots)
    ##    mat.nms
    colnames(mat.nms) <- names.dots

    ## browser()
    mat.nms <- mat.nms[do.call(order,mat.nms),]

    mat.nms
}

