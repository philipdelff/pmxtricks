##' Plot individual observations together with individual and/or population predictions
##' @description An summary plot of the observations and predictions.
##' @param data The dataset to plot.
##' @param grp character vector of columns to group means by and to facet plots by.
##' @param par.time name of column to plot observations against.
##' @param par.ntim Used if plotting population means.
##' @param log.y Use logarithmic scale for y-axis?
##' @param plot.obs Plot observations?
##' @param plot.ipre Plot individual predictions?
##' @param plot.dvmean Include mean of observations?
##' @param plot.dvrange Include range of observations?
##' @param plot.predmean Include mean of population predictions?
##' @param facet name of a column to facet by (facet_wrap used if this argument is given).
##' @param type.mean Passed to means. When linear scale is used, arithmetic is used, when on log scale, geometric is default.
##' @param debug Start by calling browser()?
##' @import data.table
##' @import ggplot2
##' @family plotting

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
    
    
    p <- ggplot(drow)
    if(plot.obs){
        p <- p + geom_point(aes_string(par.time,"DV",colour="IDwithin"))
    }
    if(plot.ipre){
        p <- p + geom_line(aes_string(par.time,"IPRED",colour="IDwithin"))
    }

    if(par.ntim%in%names(data)){
        drow[,c("DVmean","DVmean.l","DVmean.u"):=means(DV,ci=T,type=type.mean),by=c(grp,par.ntim)]
        drow[,c("PREDmean","PREDmean.l","PREDmean.u"):=means(PRED,ci=T,type=type.mean),by=c(grp,par.ntim)]
        if(plot.predmean){
            p <- p + geom_line(aes_string(par.ntim,"PREDmean"),size=1.2,inherit.aes=F)
        }
        if(plot.dvmean){
            p <- p + geom_point(aes_string(x=par.ntim,"DVmean"),size=2,colour=2)
        }
        if(plot.dvrange){
            p <- p + geom_errorbar(aes_string(x=par.ntim,ymin="DVmean.l",ymax="DVmean.u"),inherit.aes=F,colour=2)
        }
    }
    if(facet){
        p <- p + facet_wrap(as.formula(paste0("~",grp)),scales="free")
    }
    p <- p + theme(legend.position="none")

    if(log.y) p <- p+scale_y_log10()

    return(p)
    
}
