##' Order columns in dataset for use in Nonmem.
##'
##' Order the columns in a data.frame for easy export to Nonmem. Standard
##' columns like "ROW", "ID", "NTIM", etc. will be first by default, then other
##' capital case named columns, then lowercase named columns (one or more
##' lowercase letter means that column is sorted as lowercase). Except for
##' columns mentioned in "first" and "last" arguments, columns are sorted
##' alphabetically (after by case). In sohort, priority is 1: Case, first/last,
##' alphabetical. This means that last="BW" will put body weight as last of
##' capital, but before lowercase columns.
##'
##' @param first Columns that should come before alphabetic sorting. Default is c("ROW","ID","NTIM","TIME","EVID","CMT","AMT","RATE","DV","MDV","FLAG","OCC","ROUTE","GRP","TRIAL")
##' @param last Columns to sort after alphabetic ordering. Default is none.
##' @param debug Start by calling browser()?
##' @author Philip Delff
##' @export


orderColumns <- function(data,first,last,debug=F){
    if(debug) browser()
    if(missing(first)){
        first <- c("ROW","ID","NTIM","TIME","EVID","CMT","AMT","RATE","DV","MDV","FLAG","OCC","ROUTE","GRP","TRIAL")
    }
    ## others in alphebetically sorted 
    ## last is if some should be after the alphebetically ordered
    if(missing(last)){
        last <- c()
    }
    ## Then follows whatever variables contain a lowercase letter
    nms <- names(data)
### checks of existense of standard columns
    missing <- setdiff(setdiff(first,"RATE"),nms)
    if(length(missing)) warning(paste0("These standard nonmem columns were not found in data:\n",paste(missing,collapse="\n")))

    firstpts <- match(nms,c(first))
    lastpts <- match(nms,c(last))
    lowerpts <- rep(NA,length(lastpts))
    lowerpts[grep("[a-z]",names(data))] <- 1
    lowerpts[grep("[\\.]",names(data))] <- 1
    notmatched <- rowSums(cbind(firstpts,lastpts),na.rm=TRUE)==0
    middlepts <- numeric(length(firstpts))
    middlepts[which(notmatched)[order(nms[notmatched])]] <- 1:sum(notmatched)

    ord <- order(rowSums(cbind(firstpts,middlepts*1E3,lastpts*1E5,lowerpts*1e7),na.rm=TRUE))
    data.out <- data[,nms[ord]]
    data.out
}
