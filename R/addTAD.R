##' Add cumulative number of doses, time of last dose and time since last dose to data
##'
##' For now, doses have to be in data as EVID=1 and/or EVID=4
##' records. They can be in the format of one row per dose or repeated
##' dosing notation using ADDL and II.
##' @param data The data set to add the variables to.
##' @param col.time Name of time column
##' @param col.tdos
##' @param col.tad
##' @param col.ndoses
##' @param col.amt
##' @param col.evid
##' @param by
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @import data.table
##' @details This is still experimental.
##' @export

#### add NDOSES, TDOS, TAD

##' library(NMdata)
##'
##' 
## dat <- readRDS(system.file("examples/data/xgxr2.rds", package="NMdata"))
##
##

addTAD <- function(data,col.time="TIME",col.tdos="TDOS",col.tad="TAD",col.ndoses="NDOSES",col.amt="AMT",col.evid="EVID",by="ID",as.fun){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

### quit if no doses found etc
    
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)   
    }

    
    ## expand doses if necessary
    data2 <- NMexpandDoses(dat=data,quiet=TRUE)
    
    ## NDOSPERIOD
    data2[,NDOSES:=cumsum(get(col.evid)%in%c(1,4)),by=by]
    ## TDOS
    data2[EVID%in%c(1,4),TDOS:=get(col.time)]
    data2[,TDOS:=nafill(TDOS,type="locf"),by=by]
    ## TAD
    data2[,TAD:=get(col.time)-TDOS]

    setnames(data2,cc(TDOS,NDOSES,TAD),cc(col.tdos,col.ndoses,col.tad))

    data2 <- as.fun(data2)

    return(data2)

}
