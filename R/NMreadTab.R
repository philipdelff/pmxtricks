#' Read a table file from NONMEM 
#'
#' @param file path to NONMEM table file
#' @param silent logical stating whether or not information is printed.
#' @param skip Number of lines skipes in the start of the file. Some files have
#'     additional headers so if table looks wierd try adding 1 to skip. Notice
#'     that the lines TABLE 1 etc from Nonmem will be handled automatically.
#' @param sep The seperator to use in read.table. If " " is used, any number of blanks will work as one separator in the datafile. This should be OK for Nonmem output as the function is intended for but may be a problem for other tables.
#' @param debug Start by calling browser()?
#' @return Nonmem table as df.
#' @importFrom utils read.table
#' @family Nonmem
#' @export


NMreadTab <- function(file, silent=F,skip ,sep=" ",debug=F) {

    if(debug) browser()

    ## arg checks
    if(!is.character(file)) stop("file should be a character string",call.=F)
    if(!file.exists(file)) stop("argument file is not a path to an existing file.",call.=F)

    lines <- readLines(file)
    if(!missing(skip)) {
        if(!is.integer(skip)) stop("skip must be an integer.",call.=F)
        if(skip<0) stop("skip cannot be negative",call.=F)
        if(skip>0){
            lines <- lines[-(1:skip)]
        }
    }

    idx.tabh <- grep ("^TABLE.*",lines)
    idx.cnames <- idx.tabh+1

    idx.data <- setdiff(1:length(lines),c(idx.tabh,idx.cnames))
    ## idx.data

    if(sep==" ") {
        lines <- sub("^ +","",lines)
        lines <- gsub(" +",";",lines)
        sep <- ";"
        }
    
    
    data <- read.table(text=paste(lines[c(idx.cnames[1],idx.data)],collapse="\n"),sep=sep,header=T)
    data$LINENO <- idx.data

    data$TABLENO <- findInterval(data$LINENO,idx.tabh)

    data$LINENO <- NULL

    return(data)
    
}
