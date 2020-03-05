##' get an overview of your Nonmem runs
##'
##' @param dir The directory in which to search for runs
##' @param runs Runs to include in case you only want a specific subset.
##' @param runs.omit Runs to omit in case you want all but these.
##' @param debug start by running browser()?
##' @family Nonmem
##' @export

## TODO add ParNearBoundary
## TODO wrap in try.

NMrunLog <- function(dir,runs,runs.omit,debug=F){
    if(debug) browser()
    if(missing(runs)) {
        runs.lst <- sub("\\.lst$","",list.files(dir,"\\.lst$",ignore.case=T))
        runs.mod <- sub("\\.mod$","",list.files(dir,"\\.mod$",ignore.case=T))
        runs <- sort(unique(c(runs.lst,runs.mod)))
    } else {
        runs <- sub("\\.lst$|\\.mod$","",runs)
    }
    
    if(!missing(runs.omit)) {
        runs.omit <- sub("^ | $","",runs.omit)
        runs.omit <- sub("\\.lst$|\\.mod$","",runs.omit)
        runs <- setdiff(runs,runs.omit)
    }

    if(length(runs)<1) stop("No runs matched.")
    if(!missing(dir)) runs <- lapply(runs,function(run) filePathSimple(dir,run))

    runs.list <- lapply(runs,
                        function(run){
                            cat(run,"\n")
                            reslist <- try(NMreadRun(run))
                            if("try-error"%in%class(reslist)) {
                                warning("Could not read",run)
                                return(NULL)
                            } else {
                                as.data.frame(
                                    reslist[c("run","problem","Npars","OFV","run.ref","covRun","finalZeroGradient","covSuccessful","conditionNumber","Nsubjs","Nobs","minSuccessful","roundingErrors","min.problem","near.bound","grad.max","convsum","cov.request","cov.comment","covsum","conv.OK")],
                                    stringsAsFactors=F
                                )
                            }}
                        )
    
    tab <- do.call(rbind,runs.list)

    

    tab$Model <- 1:nrow(tab)
    tab[,c("Model",setdiff(colnames(tab),"Model"))]
    
}

