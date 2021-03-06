##' extract sections of Nonmem control streams
##'
##' @param file A file to read from. Normally a .mod or .lst. See lines also.
##' @param lines Text lines to process. This is an alternative to use the file
##'     argument.
##' @param section The name of section to extract. Examples: "INPUT", "PK",
##'     "TABLE", etc.
##' @param return If "text", plain text lines are returned. If "idx", matching
##'     line numbers are returned. "text" is default.
##' @param keepEmpty Keep empty lines in output? Default is FALSE.
##' @param keepName Keep the section name in output (say, "$PROBLEM") Default is
##'     TRUE. It can only be FALSE, if return"idx".
##' @param keepComments Keep comment lines?
##' @param asOne If multiple hits, concatenate into one. This will most often be
##'     relevant with name="TABLE". If FALSE, a list will be returned, each
##'     element representing a table. Default is TRUE. So if you want to process
##'     the tables separately, you probably want FALSE here.
##' @param simplify If asOne=FALSE, do you want the result to be simplified if
##'     only one table is found? Default is TRUE which is desirable for
##'     interactive analysis. For programming, you probably want FALSE.
##' @param cleanSpaces If TRUE, leading and trailing are removed, and multiplied
##'     succeeding white spaces are reduced to single white spaces.
##' @family Nonmem
##' @examples
##' NMgetSection(pmxtricks_filepath("examples/nonmem/run001.lst"),section="DATA")
##'
##' @export


NMgetSection <- function(file, lines, text, section, return="text", keepEmpty=FALSE, keepName=TRUE, keepComments=TRUE, asOne=TRUE, simplify=TRUE, linesep="\n",cleanSpaces=FALSE,debug=F){
if(debug) browser()
### check arguments
    ## if(!missing(file) & !missing(lines) ) stop("Supply either file or lines, not both")
    ## if(missing(file) & missing(lines) ) stop("Supply either file or lines.")
    if(sum(!missing(file),!missing(lines),!missing(text))!=1) stop("Exactly one of file, lines, or text must be supplied")
    if(!missing(file)) {
        if(!file.exists(file)) stop("When using the file argument, file has to point to an existing file.")
        lines <- readLines(file)
    }
    if(!missing(text)) {
        lines <- strsplit(text,split=linesep)[[1]]
    }


    
    if(!return%in%c("idx","text")) stop("text must be one of text or idx.")
    
    ## works with both .mod and .lst
    lines <-
        lines[1:(min(c(length(lines),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines)-1)))]
    
    ## Find all the lines that start with the $section
    idx.section <- grep(paste0("^ *\\$",section),lines)
    idx.dollars <- grep("^ *\\$",lines)

    ## get the sections
    idx.sections <- lapply(idx.section,function(idx.start){
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
        result <- lapply(result, function(x)sub(paste0("^ *\\$",section),"",x))
    }

    if(cleanSpaces){
        if(!return=="text") {
            stop("cleanSpaces can only be TRUE if return=='text'")
        }
        result <- lapply(result, function(x)sub(paste0("^ +"),"",x))
        result <- lapply(result, function(x)sub(paste0(" +$"),"",x))
        result <- lapply(result, function(x)sub(paste0(" +")," ",x))
    }
    
    if(asOne) {result <- do.call(c,result)}

    if(simplify && length(result)==1) result <- result[[1]]
    

########## formating return
    
    ## result <- unlist(result)
    return (result)
    
}



    ## idx or text
    ## keepName option to omit $SECTION. Only if return="text"
    ## if as.one, stack the resulting list elements
    ## list. if simplify=T and length=1, then return list[[1]]
