

NMgetSection <- function(file,lines,name,keepEmpty = FALSE, keepName = TRUE,keepComments=T,return="text",asOne=TRUE,simplify=TRUE){
    
    
### check arguments
    if(!missing(file) & !missing(lines) ) stop("Supply either file or lines, not both")
    if(missing(file) & missing(lines) ) stop("Supply either file or lines.")
    if(!missing(file)) {
        if(!file.exists(file)) stop("When using the file argument, file has to point to an existing file.")
        lines <- readLines(file)
    }
    if(!return%in%c("idx","text")) stop("text must be one of text or idx.")
    
    ## works with both .mod and .lst
    lines <-
        lines[1:(min(c(length(lines),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines)-1)))]
    
    idx.name <- grep(paste0("^ *\\$",name),lines)
    ## Find all the lines that start with the $name
    idx.dollars <- grep("^ *\\$",lines)

    ## get the sections
    idx.sections <- lapply(idx.name,function(idx.start){
        idx.dollars.after <- idx.dollars[idx.dollars>idx.start]
        if(length(idx.dollars.after)==0) {
            idx.end <- length(lines)
        } else {
            idx.end <- min(idx.dollars.after)-1
        }
        idx.section <- idx.start:idx.end
    })
    result <- idx.sections

    if(!keepEmpty){
        result <- lapply(result,function(x)
            x[!grepl("^ *$",lines[x])]
        )
    }
    
    if(!keepComments){
        result <- lapply(result,function(x)
            x[!grepl("^ *;",lines[x])]
            )
    }
    
    if(return=="text"){
        result <- lapply(result,function(x)lines[x])
    }
    
    if(!keepName){
        if(!return=="text") {
            stop("keepName can only be FALSE if return=='text'")
        }
        result <- lapply(result, function(x)sub(paste0("^ *\\$",name),"",x))
    }

    if(asOne) {result <- do.call(c,result)}

    if(simplify && length(result)==1) result <- result[[1]]
    

########## formating return
    
    ## result <- unlist(result)
    return (result)
    
}



    ## idx or text
    ## keepName option to omit $NAME. Only if return="text"
    ## if as.one, stack the resulting list elements
    ## list. if simplify=T and length=1, then return list[[1]]
