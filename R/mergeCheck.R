##' Merge, order, and check number of resulting rows.
##'
##' So far, this one can only check that we get the same no of rows as
##' the first df supplied. It is likely that much faster, better and
##' prettier functions are available out there. But for 95% of the
##' author's merges, this does exactly the checks and ordering, he
##' wants. And it only uses base R for data.frames. It also supports
##' data.tables for compatibility but if you use data.table, there are
##' faster ways to do this within that framework.
##'
##' @param df1 A data.fram with the number of rows must should be obtained from
##'     the merge. The resulting data.frame will be ordered like df1.
##' @param df2 A data.frame that will be merged onto df1.
##' @param debug Start by calling browser()?
##' @param ... additional arguments passed to merge. If all is among them, an
##'     error will be returned.
##' @family DataWrangling
##' @importFrom data.table is.data.table
##' @return a data.frame resulting from merging df1 and df2
##' @export

mergeCheck <- function(df1,df2,debug=F,...){
    if(debug) browser()
    name.df1 <- deparse(substitute(df1))
    name.df2 <- deparse(substitute(df2))
    name.df3 <- "merged.df"

    if("all"%in%names(list(...))) stop("option all not supported")

    rowcol <- tmpcol(df1)

    if(nrow(df1)) {
        reorder <- T
        if(is.data.table(df1)){
            df1[,(rowcol):=1:nrow(df1)]
        } else {
            df1[,rowcol] <- 1:nrow(df1)
        }
    } else {
        reorder <- F
    }

    df3 <- merge(df1,df2,...)
    if(nrow(df3)!=nrow(df1)){
        cat(paste0("nrow(",name.df1,"):"),nrow(df1),"\n")
        cat(paste0("nrow(",name.df2,"):"),nrow(df2),"\n")
        cat(paste0("nrow(",name.df3,"):"),nrow(df3),"\n")
        stop("merge changed dimensions")        
    }
    if(reorder){
        df3 <- df3[order(df3$rowcol),]
        df3 <- subset(df3,select=-rowcol)
    }
    
    df3

}
