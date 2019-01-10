## run <- "\\\\samba-hpc.rd.astrazeneca.net\\QCP_MODELING\\ONC\\azd5363\\poppk_20180813_Ph1Ph2Pooled_final\\Models\\baseModel\\philipdelff\\run416t14_cn_01.lst"

NMrunLogLine <- function(run){
    library(nonmem2R)
    ## for now we can assume run is a lst file
    runbase <- sub("\\.lst","",run)
    
    ext <- extload(paste0(runbase,".ext"))
    problem <- NMgetSection(paste0(runbase,".lst"),name="PROBLEM",keepDollarEmpty=F)
    problem <- sub("^ *\\$PROBLEM +","",problem)

    data.frame(Problem=problem
              ,OFV=ext$ofv
              ,Npars=length(c(ext$omega,ext$theta,ext$sigma))-length(ext$fix)
              ,stringsAsFactors=F)
}

NMrunLog <- function(runs,dir,debug=F){
    if(debug) browser()
    if(!missing(dir)) runs <- lapply(runs,function(run)filePathSimple(dir,run))
    tab <- do.call(rbind,lapply(runs,NMrunLogLine))
    tab$Model <- 1:nrow(tab)
    tab[,c("Model",setdiff(colnames(tab),"Model"))]

}

