##' Plot individual observations together with individual and/or population
##' predictions
##' @description An summary plot of the observations and predictions.
##' @param data The dataset to plot.
##' @param grp character vector of columns to group means by and to facet plots
##'     by.
##' @param col.time name of column to plot observations against.
##' @param col.ntim Used if plotting population means.
##' @param col.pred Name of column containing population predictions.
##' @param col.ipred Name of column containing individual predictions. Default
##'     is searching for IPRED, then IPRE.
##' @param log.y Use logarithmic scale for y-axis?
##' @param plot.obs Plot observations?
##' @param plot.ipred Plot individual predictions?
##' @param plot.dvmean Include mean of observations?
##' @param plot.dvrange Include range of observations?
##' @param plot.predmean Include mean of population predictions?
##' @param facet name of a column to facet by (facet_wrap used if this argument
##'     is given).
##' @param type.mean Passed to means. When linear scale is used, arithmetic is
##'     used, when on log scale, geometric is default.
##' @param debug Start by calling browser()?
##' @import data.table
##' @import ggplot2
##' @family plotting
##' @examples
##' data(pksim1,package="pmxtricks")
##' pmxtricks:::NMplotFitOld(pksim1)
##' pmxtricks:::NMplotFitOld(pksim1,debug=FALSE,plot.dvmean=TRUE,plot.dvrange=TRUE,col.ntim="TIME",log.y=TRUE)

## not ready, don't export yet

##### todo

## 2019-09-24 philipdelff: Plotting needs to be restructured. First create a long-format dataset.

##### end todo

NMplotFitOld <- function(data,grp,col.time="TIME",col.ntim="NOMTIME",col.pred="PRED",col.ipred=c("IPRED","IPRE"),log.y=FALSE,geom.obs="p",plot.ipred=TRUE,geom.dvmean="",plot.dvrange=FALSE,geom.predmean="",facet=TRUE,type.mean,debug=FALSE){

    if(debug) browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    EVID <- NULL
    IDwithin <- NULL
    ID <- NULL
    DV <- NULL
    PRED <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    drow <- copy(as.data.table(data))
    
    if(missing(grp)){
        grp <- tmpcol(drow,base="grp")
        drow[,c(grp):="All"]
    }
    

### look for ipred
    if(!is.null(col.ipred)) {
        if(length(col.ipred)==0) col.ipred <- NULL 
        found.ipred <- col.ipred %in% colnames(data)
        if(sum(found.ipred)==0) {
            message("Individual predictions not found in data. Skipping.")
            col.ipred <- NULL
            if(plot.ipred) {
                warning("plot.ipred is TRUE, but individual predictions not found in data. Skipping.")
                plot.ipred <- FALSE
            }
        } else {
            col.ipred <- col.ipred[found.ipred] [1]
        }
        
    }

### look for pred
    if(!is.null(col.pred)) {
        if(length(col.pred)==0) col.pred <- NULL 
        found.pred <- col.pred %in% colnames(data)
        if(sum(found.pred)==0) {
            message("Individual predictions not found in data. Skipping.")
            col.pred <- NULL
            if(nchar(geom.predmean)) {
                warning("geom.predmean is non-empty, but population predictions not found in data. Skipping.")
                geom.predmean <- ""
            }
        } else {
            col.pred <- col.pred[found.pred] [1]
        }
    }

    
    drow <- drow[EVID==0]
    name.idwithin <- tmpcol(drow,base="IDwithin")
    drow[,(name.idwithin):=as.numeric(as.factor(as.numeric(ID))),by=c(grp)]
    drow[,(name.idwithin):=as.factor(get(name.idwithin))]

    if(missing(type.mean)){
        if(log.y) {
            type.mean <- "geometric"
        } else {
            type.mean <- "arithmetic"
        }
    }
    
    
    p <- ggplot(drow)
### this should be handled better. What if plot.obs is longer than one etc.
    if(grepl("p",geom.obs)){
        p <- p + geom_point(aes_string(col.time,"DV",colour=name.idwithin))
    }
    if(grepl("l",geom.obs)){
        p <- p + geom_line(aes_string(col.time,"DV",colour=name.idwithin))
    }
    
    if(plot.ipred){
        p <- p + geom_line(aes_string(col.time,col.ipred,colour=name.idwithin))
    }

    if(col.ntim%in%names(data)){
        drow[,c("DVmean","DVmean.l","DVmean.u"):=means(DV,ci=T,type=type.mean),by=c(grp,col.ntim)]

        if(nchar(geom.predmean)){
            drow[,c("PREDmean","PREDmean.l","PREDmean.u"):=means(get(col.pred),ci=T,type=type.mean),by=c(grp,col.ntim)]
            if(grep("p",geom.predmean)){
                p <- p + geom_point(aes_string(col.ntim,"PREDmean"),size=1.2,inherit.aes=F)
            }
            if(grep("l",geom.predmean)){
                p <- p + geom_line(aes_string(col.ntim,"PREDmean"),size=1.2,inherit.aes=F)
            }
        }
        if(grep("p",geom.dvmean)){
            p <- p + geom_point(aes_string(col.ntim,"DVmean"),size=2,inherit.aes=F)
        }
        if(grep("l",geom.dvmean)){
            p <- p + geom_line(aes_string(col.ntim,"DVmean"),size=1.2,inherit.aes=F)
        }
        if(plot.dvrange){
            p <- p + geom_errorbar(aes_string(x=col.ntim,ymin="DVmean.l",ymax="DVmean.u"),inherit.aes=F,colour=2)
        }
    }
    if(facet){
        p <- p + facet_wrap(as.formula(paste0("~",grp)),scales="free")
    }
    p <- p + theme(legend.position="none")

    if(log.y) p <- p+scale_y_log10()

    return(p)
    
}
