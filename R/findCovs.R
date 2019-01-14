##' Extract columns that do not vary within variables in a data.frame
##'
##' Often the variables will be either empty or ID. But it can also be both say ID and DRUG or ID and GRP.
##'
##' @param data data.frame in which to look for covariates
##' @param cols.id covariates will be searched for in combinations of values in these columns
##' @param cols.drop Discard these columns if present.
##' @author philip@delff.dk
##' @export

##' @examples
##' dat <- NMimport("e:/Project/NN1406/4218/current/Nonmem/data_review/PK_NNC0143-0406/PK-01-01-1abs_1dist_comberr_RE_KA1V2CL/TABLE_OUTPUT.txt")
##' ### very common use
##' findCovs(dat,cols.id="ID",cols.drop=c("IRES","TABLE"))
##' ###  an ID column is needed.
##' findCovs(dat,cols.id=c(),cols.drop=c("IRES","TABLE"))
##' ### need a new data.frame to test for length(cols.id)>1


findCovs <- function(data,cols.id,cols.drop=NULL){

    cnames <- colnames(data)
    cnames.no.id <- setdiff(cnames,cols.id)
    cnames.to.use <- setdiff(cnames.no.id,cols.drop)

    udata <- unique(data[,cols.id,drop=FALSE])
    Nid <- nrow(udata)
    names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data[,c(cols.id,x)]))==Nid))]

    reduced.in <- unique(data[,c(cols.id,names.covs),drop=F])
    reduced.in[do.call(order,reduced.in[,cols.id,drop=F]),]
}

