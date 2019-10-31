##' Extract columns that do not vary within variables in a data.frame
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



### for a more data.table pure approach, I tried to do this. But it's horribly slow. 
    ## dt2 <- data[, .SD[, lapply(.SD, function(x)uniqueN(x)==1)], keyby=cols.id]
    ## ifkeep <- dt2[,sapply(.SD,all),.SDcols=setdiff(colnames(dt2),cols.id)]
    ## keep <- c(cols.id,setdiff(colnames(dt2),cols.id)[ifkeep])
    ## reduced <- unique(data[,keep,with=F])
### I also tried same approach as in findCovs but using data.frames. That was
### horribly slow too. For one example, the approach above was 28 secs, the one
### below based on data.frames 20 seconds, and the one above 2.3 seconds.


findCovs <- function(data,cols.id=NULL,debug=F){
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

    cnames <- colnames(data)
    cnames.to.use <- setdiff(cnames,cols.id)
    
    if(is.null(cols.id)){
        Nid <- 1
    } else {
        Nid <- nrow(unique(data[,cols.id,with=F]))
    }


    names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data[,c(cols.id,x),with=F]))==Nid))]

    
    reduced <- unique(data[,c(cols.id,names.covs),with=F])
    if(!is.null(cols.id)){
        reduced <- reduced[order(get(cols.id))]
    }

    if(!was.data.table) reduced <- as.data.frame(reduced)
    reduced

}

