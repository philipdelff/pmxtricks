##' A standard panel of residual plots
##' 
##' @param data is all rows from the Nonmem output. Only the EVID==0
##'     subset will be used.
##' @param res The individual residuals (how to derive them from data)
##' @param ipre expression for individual predictions
##' @param time Time variable (normally TIME or NOMTIME or TAPD)
##' @param colour expression for colour mapping
##' @param title An optional character title for the plots.
##' @param arrange If true, the plots will be arranged with
##'     arrangeGrob. If not, they will be returned as individual plots
##'     in a list.
##' @param debug Start by runing bowser?
##' @details All parameters must be given as expressions (no quotes)
##' @family Plotting
##' @importFrom rlang enquo
##' @import patchwork
## @importFrom egg ggarrange


##### Don't export yet. Needs to be elaborated a bit.


### user needs to be able to get the combinations asked for and not the full list of plots

### support for better labels needed

NMplotGOF <- function(data,res=RES,pred=PRED,ipre=IPRED,ires=IRES,iwres=IWRES,time=TIME,tspd=TSPD,cwres=CWRES,colour=NULL,arrange=T,title,debug=F){
    
    if(debug) browser()


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    CWRES <- NULL
    IPRED <- NULL
    TIME <- NULL
    EVID <- NULL
    PRED <- NULL
    IWRES <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    ##    if(is.character(res)){
    ##        res=sym(res)
    ##    } else {
##    str.res <- res
    res=enquo(res)
    ires=enquo(ires)
    ##    }
    pred=enquo(pred)
    ipre=enquo(ipre)
    cwres=enquo(cwres)
    iwres=enquo(iwres)
    time=enquo(time)
    colour=enquo(colour)
    
    data <- subset(data,EVID==0)

    plots <- list()
### PRED/RES plots
    plots$res.pred <- ggplot(data,aes(!!pred,!!res,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    plots$res.time <- ggplot(data,aes(!!time,!!res,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)

### IPRED/IRES plots
    if(quo_name(ires) %in% colnames(data)){
        plots$ires.ipre <- ggplot(data,aes(!!ipre,!!ires,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
        plots$ires.time <- ggplot(data,aes(!!time,!!ires,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    }

    ## IWRES
    plots$iwres.ipre <- ggplot(data,aes(!!ipre,!!iwres,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    plots$iwres.time <- ggplot(data,aes(!!time,!!iwres,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    
    ## CWRES
    if(quo_name(cwres) %in% colnames(data)){
        plots$cwres.ipre <- ggplot(data,aes(!!ipre,!!cwres,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
        plots$cwres.time <- ggplot(data,aes(!!time,!!cwres,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    }
    
    if(arrange){
        ## egg::ggarrange
        if(missing(title)) title <- ""
        ## all.ps <- ggarrange(pred.res,
        ##                     ipre.res,
        ##                     ipre.iwres,
        ##                     time.cwres
        ##                    ,time.iwres
        ##                    ,time.res
        ##                    ,nrow=2
        ##                    ,top=title)
        ## based on patchwork
        ## all.ps <-
        ##     res.pred +
        ##     res.time +
        ##     ires.ipre +
        ##     ires.time +
        ##     iwres.ipre +
        ##     iwres.time +
        ##     cwres.ipre +
        ##     cwres.time +
        ##     plot_layout(nrow = 2,byrow=FALSE) +
        ##     plot_annotation(title = title)

        all.ps <- Reduce(`+`,plots)+
            plot_layout(nrow = 2,byrow=FALSE,guides = 'collect') +
            plot_annotation(title = title)+
            plot_layout(guides = 'collect') 
        

    } else {
        ## all.ps <- list(res.pred = res.pred,
        ##                res.time = res.time,
        ##                ires.ipre = ires.ipre,
        ##                ires.time = ires.time,
        ##                iwres.ipre = iwres.ipre,
        ##                iwres.time = iwres.time,
        ##                cwres.ipre = cwres.ipre,
        ##                cwres.time = cwres.time
        ##                )
        all.ps <- plots
        
        if(!missing(title)) all.ps <- lapply(all.ps,labs(subtitle=title))
    }
    all.ps
}
