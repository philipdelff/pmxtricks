##' Extract columns that vary within values of other columns in a data.frame
##'
##' @param data data.frame in which to look for covariates
##' @param cols.id covariates will be searched for in combinations of values in
##'     these columns. Often cols.id will be either empty or ID. But it
##'     can also be both say c("ID","DRUG") or c("ID","TRT").
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


findVars <- function(data,cols.id=NULL,debug=F){
    if(debug) browser()
    ## check arguments
    if(!is.data.frame(data)){
        stop("data must be a data.frame (or data.table)")
    }
    
    was.data.table <- T
    if(!is.data.table(data)){
        was.data.table <- F
        data <- as.data.table(data)
    }

    ## uniqueN > 1
    dt2 <- data[, .SD[, lapply(.SD, function(x)uniqueN(x)>1)], by=cols.id]
    ## use any
    ifkeep <- dt2[,sapply(.SD,any),.SDcols=!(cols.id)]
    keep <- c(cols.id,setdiff(colnames(dt2),cols.id)[ifkeep])
    reduced <- unique(data[,keep,with=F])

    if(!was.data.table) reduced <- as.data.frame(reduced)
    reduced

}

