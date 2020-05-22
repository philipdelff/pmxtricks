##' Print a meta.data object
##'
##' @param data The dataset being described (i.e. a data.frame).
##' @param meta.data The existing meta.data object (see \code{metaInit}).
##' @param file Path to text file to write to. If not provided,
##'     meta.data is printed to screen.
##' @param match.data Reorder meta.data to match data? If this function call
##'     is used for saving metadata right after saving the data file, this option
##'     should be used to make sure that the metadata file will match the dataset
##'     as well as possible.
##' @param silent If silent, the metadata is not printed to the screen.
##' @param test Nice for seeing what the meta data will look like. Problems
##'     will yield warnings, not errors.
##' @return The meta data object printed as formatted text
##' @export
##' @family DataGen
##' @author PPDA

###### Change log ######
## PPDA: introduced match.data argument. When TRUE, metadata is sorted
## to match the order of the data.

####### End change log

####### TODO #########
##
#### END TODO ####

metaPrint <- function(data,meta.data,file=NULL,fileData,match.data=T,silent=!is.null(file),test=F,debug=F){
    if(debug) browser()

    datacols <- colnames(data)
    if(match.data){
        ## Sort meta
        meta.data$variables <- meta.data$variables[order(match(meta.data$variables$variable,datacols)),]
        rownames(meta.data$variables) <- 1:nrow(meta.data$variables)
    }
    
    if(test){
        checkResult <- metaCheck(data,meta.data,fileData=fileData,match.data=T,silent=T)
    } else {
        stopifnot(metaCheck(data,meta.data,fileData=fileData,match.data=T,silent=T))
    }

    MetaFile.header.lines <- c(
        ifelse(missing(fileData),NULL,paste("Meta data for NONMEM data file:",fileData)),
        paste("Data file created by script:",meta.data$meta$DataCreateScript),
        paste("Data file created for trial:",meta.data$meta$Trial),
        paste("This file was created:",Sys.time())
    )
### Assumption: There are exactly three required fields (plus
### time) in meta.dat$meta.
    if(length(meta.data$meta)>3){
        MetaFile.header.lines <- c(MetaFile.header.lines,unlist(meta.data$meta[-c(1:3)]))
    }

    MetaFile.header.lines <- c(MetaFile.header.lines,"        
***********************************************************************
Variables and units used in data file :
***********************************************************************",sep="")

    MetaFile.header <- paste(MetaFile.header.lines,collapse="\n")

### Formatting listing of variables. Code should be simplified.
    max.chars.var <- max(nchar(as.character(meta.data$variables$variable)))
    nchars.reserved <- max.chars.var+3
    fmtstring <- paste("% -",max.chars.var,"s",sep="")
    
    ## MetaFile.variables.all <- mutate(meta.data$variables,
    ##                              text.unit=ifelse(is.na(unit),"",paste(" (",unit,")",sep="")),
    ##                              lines.values=gsub('(.{1,60})(\\s|$)', paste('\\1\n',paste(rep(" ",nchars.reserved),collapse=""),sep=""), values)
    ##                              )
    MetaFile.variables.all <- transform(meta.data$variables,
                                        text.unit=ifelse(is.na(unit),"",paste(" (",unit,")",sep="")),
                                        lines.values=gsub('(.{1,60})(\\s|$)', paste('\\1\n',paste(rep(" ",nchars.reserved),collapse=""),sep=""), values),
                                        stringsAsFactors=FALSE
                                        )
    MetaFile.variables.all$lines.values <- sub("\n *$","",MetaFile.variables.all$lines.values)

    MetaFile.variables.all$text.values=""
    for(I in 1:nrow(MetaFile.variables.all)){
        if(!is.na(MetaFile.variables.all[I,"values"])){
            MetaFile.variables.all$text.values[I] <- paste("\n",paste(rep(" ",nchars.reserved),collapse=""),
                                                           MetaFile.variables.all[I,"lines.values"],sep="")
        }
        
    }
    
    ## MetaFile.variables.all <- mutate(MetaFile.variables.all,
    ##                                  text=paste(sprintf(fmtstring,variable)," - ",description, text.unit,text.values,sep="")
    ##                                  )
    MetaFile.variables.all$text <- with(MetaFile.variables.all,
                                        paste(sprintf(fmtstring,variable)," - ",description, text.unit,text.values,sep="")
                                        )
    

    MetaFile.variables <- MetaFile.variables.all$text
    

    MetaDataContents <- paste(MetaFile.header,paste(MetaFile.variables,collapse="\n"),sep="")
    
    if(!is.null(file)){
        f <- file(file,open="wb")
        cat(MetaDataContents,"\n",file=f)
        close(f)
        cat("meta data exported to",file,"\n")
    }

    if(!silent) {
        cat(MetaDataContents,"\n")
    } 
    invisible(MetaDataContents)
    
}
