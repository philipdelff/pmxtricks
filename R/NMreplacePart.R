##' replace ("dollar") parts of a nonmem control stream
##'
##' Just give the dollar name, the new lines and the file path, and the
##' "dollar section", and the input to Nonmem will be updated.
##'
##' @param path The run to edit. If a directory is given, the file is assumed to
##'     be called input.txt in that folder.
##' @param dollar The name of the dollar section to update. Example:
##'     dollar="EST". See findDollar.
##' @param newlines The new text. Better be broken into lines in a character
##'     vector since this is simply written with writeLines.
##' @param newpath path to new run. If missing, path is used. If NULL, output is
##'     returned rather than written.
##' @param backup In case you are overwriting the old file, do you want to
##'     backup the file (to say, backup_input.txt)?
##' @param test Want to see the resulting input.txt and not write it to disk? Default is FALSE.
##' @param debug start by running browser()?
##'
##' @examples
##' newlines <- "$EST POSTHOC INTERACTION METHOD=1 NOABORT PRINT=5 MAXEVAL=9999 SIG=3"
##' nmReplacePart(path=file.nonmem("FtfpiConc/10_01"),
##' dollar="EST",
##' newlines=newlines)


NMreplacePart <- function(path,dollar,newlines,newpath,backup=T,blank.append=T,test=F,debug=F){
    if(debug) browser()

#### handle arguments
    path <- filePathSimple(path)
    file <- path
    if(dir.exists(path)) file <- file.path(path,"input.txt")
    stopifnot(file.exists(file))

    if(missing(newpath)) newpath <- path
    if(!is.null(newpath)){
        newfile <- filePathSimple(newpath)
        if(dir.exists(newpath)) newfile <- file.path(newpath,"input.txt")
        stopifnot(file.exists(newfile))
    }

    ### a \n means a new line, so split by that
    newlines <- strsplit(newlines,"\n")[[1]]
    if(blank.append) newlines <- c(newlines,"")
    
######
    


    
    lines <- readLines(file)
    idx.dlines <- findDollar(lines,name=dollar,keepEmpty=T,keepDollarEmpty=T,drop.comments=F)

    stopifnot(length(idx.dlines)>0)
    
    if(length(idx.dlines)>1) {
        ## if th
        stopifnot(max(diff(idx.dlines))==1)
    }
    min.dl <- min(idx.dlines)
    max.dl <- max(idx.dlines)

    ### these two cases need to be handled slightly differently so not now
    stopifnot(min.dl>1)
    stopifnot(max.dl<=length(lines))

    if(min.dl>1){
        newlines <- c(lines[1:(min.dl-1)],
                      newlines)
    }
    if(max.dl<length(lines)){
        newlines <- c(newlines,
          lines[(max.dl+1):length(lines)])
    }

    if(is.null(newpath)) return(newlines)

    if(file==newfile && backup ) file.copy (file,
                                            sub("(.+/)([^/].+$)","\\1backup_\\2",x=file)
                                            )

    if(test){
        return(newlines)
    }
    writeLines(newlines,con=newfile)
    
    
}
