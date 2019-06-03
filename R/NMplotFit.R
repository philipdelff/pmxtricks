## not ready, don't export yet

NMplotFit <- function(data,grp="dose",par.time="TIME",par.ntim="NOMTIME",log.y=F,plot.obs=T,plot.ipre=T,plot.dvmean=F,plot.dvrange=F,plot.predmean=F,facet=T,type.mean,debug=F){

    if(debug) browser()
    drow <- data.table(data)
    drow <- drow[EVID==0]    
    drow[,IDwithin:=as.numeric(as.factor(as.numeric(ID))),by=c(grp)]


    drow[,IDwithin:=factor(IDwithin)]

    if(missing(type.mean)){
        if(log.y) {
            type.mean <- "geometric"
        } else {
            type.mean <- "arithmetic"
        }
    }
    
    drow[,c("DVmean","DVmean.l","DVmean.u"):=means(DV,ci=T,type=type.mean),by=c(grp,par.ntim)]
    drow[,c("PREDmean","PREDmean.l","PREDmean.u"):=means(PRED,ci=T,type=type.mean),by=c(grp,par.ntim)]
    
    p <- ggplot(drow)
    if(plot.obs){
        p <- p + geom_point(aes_string(par.time,"DV",colour="IDwithin"))
    }
    if(plot.ipre){
        p <- p + geom_line(aes_string(par.time,"IPRED",colour="IDwithin"))
    }
    if(plot.predmean){
        p <- p + geom_line(aes_string(par.ntim,"PREDmean"),size=1.2,inherit.aes=F)
    }
    if(plot.dvmean){
        p <- p + geom_point(aes_string(x=par.ntim,"DVmean"),size=2,colour=2)
    }
    if(plot.dvrange){
        p <- p + geom_errorbar(aes_string(x=par.ntim,ymin="DVmean.l",ymax="DVmean.u"),inherit.aes=F,colour=2)
    }
    if(facet){
        p <- p + facet_wrap(as.formula(paste0("~",grp)),scales="free")
    }
    p <- p + theme(legend.position="none")

    if(log.y) p <- p+scale_y_log10()

    return(p)
    
}
