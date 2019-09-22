##' A standard panel of residual plots
##' @param data is all rows from the Nonmem output. Only the EVID==0 subset will
##'     be used.
##' @param res The individual residuals (how to derive them from data)
##' @param ipre expression for individual predictions
##' @param time Time variable (normally TIME or NOMTIME or TAPD)
##' @param arrange If true, the plots will be arranged with arrangeGrob. If not, they will be returned as individual plots in a list.
##' @param debug Start by runing bowser?
##' @details All parameters must be given as expressions (no quotes)
##' @import rlang
##' @importFrom gridExtra arrangeGrob

##### Don't export yet. Needs to be elaborated a bit.


NMplotGOF <- function(data,res=CWRES,ipre=IPRED,time=TIME,arrange=T,debug=F){
    
    if(debug) browser()

    ##    if(is.character(res)){
    ##        res=sym(res)
    ##    } else {
    res=enquo(res)
    ##    }
    ipre=enquo(ipre)
    time=enquo(time)
    
    data <- subset(data,EVID==0)

    
    p1 <- ggplot(data,aes(PRED,!!res))+geom_point()+geom_smooth(colour=2)
    p2 <- ggplot(data,aes(!!ipre,!!res))+geom_point()+geom_smooth(colour=2)
    p3 <- ggplot(data,aes(!!ipre,IWRES))+geom_point()+geom_smooth(colour=2)
    p4 <- ggplot(data,aes(!!time,CWRES))+geom_point()+geom_smooth(colour=2)
    
    if(arrange){
        all.ps <- arrangeGrob(p1,p2,p3,p4,ncol=2)
    } else {
        all.ps <- list(p1,p2,p3,p4)
    }
    all.ps
}
