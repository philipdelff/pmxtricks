##' A standard panel of residual plots
##' 
##' @param data is all rows from the Nonmem output. Only the EVID==0
##'     subset will be used.
##' @param res Name of column with the individual residuals 
##' @param ipre column for individual predictions
##' @param time Time variable (normally TIME or NOMTIME or TAPD)
##' @param title An optional character title for the plots.
##' @param arrange If TRUE, the plots will be arranged with
##'     wrap_plots. If not, they will be returned as individual plots
##'     in a list.
##' @param debug Start by runing bowser?
##' @param ... passed to aes_string. Example colour="dose".
##' @details All parameters must be given as expressions (no quotes)
##' @family Plotting
##' @import patchwork


##### Don't export yet. Needs to be elaborated a bit.


### support for better labels needed


NMplotGOF <- function(data,res="RES",pred="PRED",ipre="IPRED",ires="IRES",iwres="IWRES",time="TIME",tspd="TSPD",cwres="CWRES",arrange=TRUE,title,smooth=TRUE,debug=F,...){
    
    if(debug) browser()

    data <- subset(data,EVID==0)
    cnames.data <- colnames(data)
    
    p1 <- ggplot(data)+
        geom_point(aes_string(...))
    if(smooth){
        p1 <- p1 + geom_smooth(aes_string(...),method="loess",formula=y~x,se=FALSE)
    }

    update.plot <- function(plot,x,y){
        if( is.null(x) || is.null(y) || any(!c(x,y)%in%cnames.data)){
            return(NULL)
        } else {
            plot + aes_string(x=x,y=y)
        }
    }
    plots <- list()
    
### PRED/RES plots
    plots$res.pred <- update.plot(p1,pred,res)
    plots$res.time <- update.plot(p1,time,res)
    plots$res.tspd <- update.plot(p1,tspd,res)

### IPRED/IRES plots
    plots$ires.ipre <- update.plot(p1,ipre,ires)
    plots$ires.time <- update.plot(p1,time,ires)
    plots$ires.tspd <- update.plot(p1,tspd,ires)

### IWRES
    plots$iwres.ipre <- update.plot(p1,ipre,iwres)
    plots$iwres.time <- update.plot(p1,time,iwres)
    plots$iwres.tspd <- update.plot(p1,tspd,iwres)

### CWRES
    plots$cwres.ipre <- update.plot(p1,ipre,cwres)
    plots$cwres.time <- update.plot(p1,time,cwres)
    plots$cwres.tspd <- update.plot(p1,tspd,cwres)

    
    if(arrange){
        if(missing(title)) title <- ""

        ## all.ps <- Reduce(`+`,plots)
        all.ps <- wrap_plots(plots)
        
        nrow <- sum(c(pred,time,tspd)%in%cnames.data)
        
        all.ps <- all.ps +
            plot_layout(nrow = nrow,byrow=FALSE,guides = 'collect') +
            plot_annotation(title = title)
        

    } else {

        all.ps <- plots
        
        if(!missing(title)) all.ps <- lapply(all.ps,labs(subtitle=title))
    }
    all.ps
}
