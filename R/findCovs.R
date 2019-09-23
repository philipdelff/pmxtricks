##' Extract columns that do not vary within variables in a data.frame
##'
##' @param data data.frame in which to look for covariates
##' @param cols.id covariates will be searched for in combinations of values in
##'     these columns. Often cols.id will be either empty or ID. But it
##'     can also be both say c("ID","DRUG") or c("ID","TRT").
##' @param cols.drop Discard these columns if present.
##' @param debug start by running browser()?
##' @family DataWrangling
##' @import data.table
##' @export

##' @examples
##' \dontrun{
##' dat <- NMreadTab("TABLE_OUTPUT.txt")
##' ### very common use
##' findCovs(dat,cols.id="ID",cols.drop=c("IRES","TABLE"))
##' ###  an ID column is not needed.
##' findCovs(dat,cols.id=c(),cols.drop=c("IRES","TABLE"))
##' ### need a new data.frame to test for length(cols.id)>1
##' }


## this should be faster with data.table. This can be used (but does not work with cols.id):
## DT = as.data.table(data)
## unique(DT[,sapply(DT,function(x)length(unique(x))==1),with=F])

findCovs <- function(data,cols.id=NULL,cols.drop=NULL,debug=F){
    if(debug) browser()

    was.data.table <- F
    if(is.data.table(data)){
        was.data.table <- T
        data <- as.data.frame(data)
    }
    
    cnames <- colnames(data)
    cnames.no.id <- setdiff(cnames,cols.id)
    cnames.to.use <- setdiff(cnames.no.id,cols.drop)

    ### do this as data.frame in case cols.id is multiple columns
    if(is.null(cols.id)){
        Nid <- 1
    } else {
        udata <- unique(data[,cols.id,drop=FALSE])
        Nid <- nrow(udata)
    }

    names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data[,c(cols.id,x),drop=F]))==Nid))]

    
    reduced.in <- unique(data[,c(cols.id,names.covs),drop=F])
    if(!is.null(cols.id)){
        reduced.in <- reduced.in[do.call(order,reduced.in[,cols.id,drop=F]),]
    }

    if(was.data.table) reduced.in <- as.data.table(reduced.in)
    reduced.in

}

