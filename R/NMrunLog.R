
## TODO add ParNearBoundary

NMrunLog <- function(runs,dir,debug=F){
    if(debug) browser()
    if(!missing(dir)) runs <- lapply(runs,function(run) filePathSimple(dir,run))

    
    tab <- do.call(rbind,lapply(runs,
                                function(run){
                                    reslist <- NMreadRun(run)
                                    as.data.frame(
                                        reslist[c("run","problem","Npars","OFV","run.ref","covRun","finalZeroGradient","covSuccessful","conditionNumber","Nsubjs","Nobs","minSuccessful","roundingErrors")])
                                }
                                ))
    
    tab$Model <- 1:nrow(tab)
    tab[,c("Model",setdiff(colnames(tab),"Model"))]
    
}

