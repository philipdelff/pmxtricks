##' read input data and translate names according to the $INPUT section
##' 
##' @description Based on a nonmem run, this function finds the input data and reads it. But it reads it like the nonmem run by applying DROP arguments and alternative naming of columns in the nonmem run.
##' @param file a .lst or a .mod. No matter which you provide, the .mod is required and is the only one to be read.
##' @param debug start by running browser()?
##' @family Nonmem
##' @export

NMtransInput <- function(file,debug=F){

if(debug) browser()
    
### the lst file only contains the name of the data file, not the path to it. So we need to find the .mod instead.
    if(grepl("\\.lst$",file)) file <- sub("\\.lst","\\.mod",file)

    lines <- NMgetSection(file,section="INPUT",keepName=F)
    lines

    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)

    nms <- strsplit(line," ")[[1]]

    ## find all equals. drop or rename?
    drop <- grepl("= *DROP",nms)
### this is if they are not intended to be kept.
    ## nms <- sub(".*=(.*)","\\1",nms)
    ## nms[nms=="DROP"] <- paste0("DROP",1:sum(nms=="DROP"))
### this is to keep even dropped columns
    nms <- sub("(.*) *= *DROP","\\1",nms)
    nms <- sub(".*=(.*)","\\1",nms)

    ## translateInput <- function()
    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMgetSection(file,section="DATA",keepName=F,keepComments=F,keepEmpty=F)

    ## pick $DATA and the next string
    lines.data2 <- paste(lines.data,collapse=" ")
    path.data.input <- sub(" *([^ ]+) +.*","\\1",lines.data2)

    pathIsAbs <- function(path) grepl("^/",path)
    if(pathIsAbs(path.data.input)){
### assuming we are on windows
        stop("absolute paths not supported")
        path.data.input <- (path.data.input)
    } else {
        path.data.input <- file.path(dirname(file),path.data.input)
    }


    path.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",path.data.input)
    if(file.exists(path.data.input.rds)){
        message("found rds input file. This will be used.")
        path.data.input <- path.data.input.rds
        data.input <- readRDS(path.data.input)
    } else {
        if(file.exists(path.data.input)){
            message("Found input data file. Reading with NMreadCsv")
            data.input <- NMreadCsv(path.data.input)
        } else {
            stop(paste("Input data file not found. Was expecting to find",path.data.input))
    ##        use.input <- FALSE
        }
    }
    

    cnames.input <- colnames(data.input)
    cnames.input[1:length(nms)] <- nms
    colnames(data.input) <- cnames.input
    data.input

}
