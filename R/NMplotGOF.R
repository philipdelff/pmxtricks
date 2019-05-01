##' data is all rows

NMplotGOF <- function(data,res=CWRES,ipre=IPRED,time=TIME,debug=F){
    
    if(debug) browser()

    library(dplyr)
    library(gridExtra)


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
    
    ## grid.arrange(p1,p2,p3,p4,ncol=2)
    all.ps <- arrangeGrob(p1,p2,p3,p4,ncol=2)
    all.ps
}
