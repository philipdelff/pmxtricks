##' factor with automated levels by sorting a corresponding variable
##' @details Use base::reorder instead. I think it does the same. factor2 is
##'     sometimes convenient, but it does not work within data.table.
##' @param x The variable make a factor variable out of.
##' @param by The variable which values the levels of the resulting values will
##'     be sorted by.
##' @param debug start by calling browser()?
##' @import data.table
##' @family DataWrangling

## Don't export - use reorder instead. reorder adds a scores attribute
## which is annoying. But I don't think factor2 can be used inside
## data.tables which makes it even more annoying. Maybe just create a
## wrapper that removes the scores attribute after running reorder?

factor2 <- function(x,by=x,debug=F){
    if(debug) browser()

    DT1 <- data.table(x=x,by=by)
    DT1[,rank:=as.numeric(as.factor(frank(by,ties.method="min")))]

    DT2 <- unique(DT1)

    factor(x,levels=DT2[order(rank),x])
}
