##' Check consistency of a meta data object.
##'
##' @param data The dataset being described (i.e. a data.frame).
##' @param meta.data The existing meta.data object (see
##'     \code{metaInit}).
##' @param file.data Path to the data file.
##' @param match.data Check the metadata against the dataset? This is
##'     default and normally a main reason to run this function.
##' @param skip.chars Only look at numeric variables in data? Default
##'     is TRUE.
##' @param silent Print to terminal whether checks passed. If FALSE,
##'     this info will still be in the returned value.
##' @param debug Start by calling browser().
##' @return A logical variable whether all checks passed.
##' @importFrom utils capture.output
##' @export
##' @family DataGen


metaCheck <- function(data,meta.data,file.data,match.data=TRUE,skip.chars=T,silent=F,debug=F){
    if(debug) browser()
    if(!is.data.frame(data)){stop("data must be a data.frame")}
    data <- as.data.frame(data)

    ##    if(skip.chars) data <- data[,unlist(lapply(data,is.numeric))]
    if(skip.chars){
        data <- data[,
                     unique(c(
                         which(unlist(lapply(data,NMisNumeric)))
                        ,
                         which(colnames(data)%in%meta.data$variables$variable)
                     ))]
    }

    
    datacols <- colnames(data)
    ## check that data file is given

    if(missing(file.data)||
       !is.character(file.data)||
       file.data==""||
       is.na(file.data)){
        stop("meta.data does not contain NONMEM data file")
    }
    ## check that data creation script is given
    if(is.null(meta.data$meta[["DataCreateScript"]])||
       !is.character(meta.data$meta[["DataCreateScript"]])||
       meta.data$meta[["DataCreateScript"]]==""||
       is.na(meta.data$meta[["DataCreateScript"]])){
        stop("meta.data does not contain data creation script.")
    }
    
    OK <- T
    if(match.data){
        ## check that all columns in data are documented in meta
        if(any(!datacols%in%meta.data$variables$variable)){
            warning(paste("The following data columns are not documented in meta.data:\n",paste(datacols[!datacols%in%meta.data$variables$variable],collapse="\n"),sep=""))
            OK <- F
        }
        ## check that nothing is documented in meta data without being in data
        if(any(!meta.data$variables$variable%in%datacols)){
            warn <- paste("The following variables documented in meta data are not found in the dataset:\n",paste(meta.data$variables$variable[!meta.data$variables$variable%in%datacols],collapse="\n"),sep="")
            if(skip.chars) {
                warn <- paste0(warn,"\nIt could also be that the variable(s) is/are present in data but that they are not numeric")
            }
            warning(warn)
            
            OK <- F
        }
    }
    ## Check that all variables in meta are unique
    if(sum(duplicated(meta.data$variables$variable))){
        warning(paste("The following entries in meta.data refer to non-unique variables in the dataset:\n",capture.output(print(meta.data$variables$variable[duplicated(meta.data$variables$variable)]),collapse="")),"\nThis should not happen if you use only the metaInit and metaAdd functions to generate the meta data. If it did, you may have found a bug. Please report.")
        OK <- F
    }
    
    if(!silent) {
        if(OK) {
            message("All checks passed.")
        } else {
            message("Not all checks passed (see warnings).")
        }
    }
    
    invisible(OK)
}
