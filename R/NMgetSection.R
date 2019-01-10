#' Extract a specific section from Nonmem code
#' 
#' @param lines A Nonmem output or input file as a vector of lines
#' @param name The desired section name. eg PROBLEM (PROB) without the
#'     $
#' @param keepEmpty Boolean. Should empty lines be included? Defaults
#'     to FALSE.
#' @param keepDollarEmpty Boolean. Should a line including the dollar,
#'     but with nothing else be kept? Defaults to TRUE.
#' @param dropComments drop comment lines from output.
#' @param return "text" or "lineNumbers". What do you want to get back
#'     from the function? The actual text or the line numbers to find
#'     the text?
#' @return The value of desired $name with comments and everything.
#'
#' @examples
#' testLines = list("$asdf  ","$asdf asd ", "asdfa","$bla", "$derp","")
#' NMgetSection(testLines,"bla")
#' NMgetSection(testLines,"asdf")
#' NMgetSection(testLines,"asdf", FALSE, FALSE)
#' NMgetSection(testLines,"derp",TRUE)


####### TODO
### the function only works reliably on input.txt, not output.txt. I suggest discarding the possibly Nonmem generated part of the lines (which is the part after the repeated input.txt). So far, I have used something like this to remove it:
## lines <- lines[1:(min(c(length(lines.tab),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines)-1)))]
### it may have to be adjusted not to make trouble if grep returns nothing.

####### end todo


NMgetSection <- function(file,lines,name,keepEmpty = FALSE, keepDollarEmpty = TRUE,dropComments=T,return="text"){

### check arguments
    if(!missing(file) & !missing(lines) ) stop("Supply either file or lines, not both")
    if(missing(file) & missing(lines) ) stop("Supply either file or lines.")
    if(!missing(file)) {
        if(!file.exists(file)) stop("When using the file argument, file has to point to an existing file.")
        lines <- readLines(file)
    }
    
    ## Result array
    result = c()
    regex = paste("^ *\\$",name,sep = "")
    ## Find all the lines that start with the $name
    findLines = grep(regex,lines)
    ## For each found line, check consecutive lines
    for (line in findLines){
        ## First of all add this $name if it's not empty
        ## if(nchar(sub(paste("^\\$",name," *",sep = ""),"",lines[line]))> 0)
        ## If there is only empty characters after "$name", then check if it should be added to result
        if (keepDollarEmpty || !grepl(paste(regex,"[[:space:]]*$",sep = ""),lines[line])){
            result = c(result,line)
        }
        i = line + 1
        ## Check a line at a time after the previous if they start with $something. If they do not, then add it to the result
        while( i <= length(lines) && !grepl("^\\$[A-Za-z]+",lines[i])){
            ## Check if it's not empty, before adding it. This removes useless lines
            if(keepEmpty) result = c(result,i)
            else {
                                        # if (nchar(lines[i]) > 0) result = c(result,i)
                ## check that line is not just spaces
                if (!grepl("^[[:space:]]*$",lines[i])) result = c(result,i)
            }
            i = i + 1
        }
    }
    ## dropping comments
    if(dropComments){
        result <- result[!grepl("^ *;",lines[result])]
    }
    result <- unlist(result)
    if(return=="text") result <- lines[result]
    return (result)
}
