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


NMplotGOF <- function(data,res=CWRES,ipre=IPRED,time=TIME,colour=NULL,arrange=T,title,debug=F){
    
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
    res=enquo(res)
    ##    }
    ipre=enquo(ipre)
    time=enquo(time)
    colour=enquo(colour)
    
    data <- subset(data,EVID==0)

    
    pred.res <- ggplot(data,aes(PRED,!!res,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    ipre.res <- ggplot(data,aes(!!ipre,!!res,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    ipre.iwres <- ggplot(data,aes(!!ipre,IWRES,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    time.cwres <- ggplot(data,aes(!!time,CWRES,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    time.iwres <- ggplot(data,aes(!!time,CWRES,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    time.res <- ggplot(data,aes(!!time,RES,colour=!!colour))+geom_point()+geom_smooth(method="loess",formula=y~x,se=FALSE)
    
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
        all.ps <- pred.res +
            ipre.res +
            ipre.iwres +
            time.cwres +
            time.iwres +
            time.res +
            plot_layout(nrow = 2) +
            plot_annotation(title = title)

    } else {
        all.ps <- list(pred.res=pred.res
                      ,ipre.res=ipre.res,
                       ipre.iwres=ipre.iwres,
                       time.cwres=time.cwres,
                       time.iwres=time.iwres,
                       time.res=time.res
                       )
        if(!missing(title)) all.ps <- lapply(all.ps,labs(subtitle=title))
    }
    all.ps
}
