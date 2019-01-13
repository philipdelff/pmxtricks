## run <- "\\\\samba-hpc.rd.astrazeneca.net\\QCP_MODELING\\ONC\\azd5363\\poppk_20180813_Ph1Ph2Pooled_final\\Models\\baseModel\\philipdelff\\run416t14_cn_01.lst"

## for inspiration, from Pirana design:
## "003" = list (
## " modelfile " = "003. mod " ,
## " description " = " PK model digoxin " ,
## " reference_model " = "002" ,
## " data_file " = " nm_pk_001 . csv " ,
## " output_file " = "003. lst " ,
## " tables " = c ("003. TAB " , " sdtab003 ")
## )

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

