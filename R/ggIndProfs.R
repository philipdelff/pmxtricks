##### INDIVIDUAL PLOTS IN LIST - before secondary axis implement

#### todo

## the two for loops to control faceting is horribly inflexible. Do the plot, then facet. First: for splitting into pages, introduce a variable (column) that groups into the splitting needed. 

## only plot available variables. Warn users what not found if verbose?

## there must be something easier than the format data stuff 

## structure the evid selection

## optionally colour all by id

## In yrange LLOQ should be taken into account if provided

### end todo


##' @param run The main title of the plot. Called run becaus you often want a Nonmem run name here.
##' @param id The name of the subject ID column
##' @param use.evid2 Should EVID 2 records be used for pred and ipred plotting? The default is to use EVID==2 records if data contains a column called EVID, and this column has at least one value equalling 2.
##' @param facet splits plots in pages
##' @param par.prof Distinguish multiple profiles in dataset. 
ggIndProfs <- function(data, run = "", x="TIME", dv="DV", pred="PRED", ipred="IPRE", grp = "GRP", amt = "AMT", id = "ID", xlab = NULL, ylab = NULL, ylab2 = NULL, free.lims = F, logy = F, NPerSheet=12,LLOQ=NULL, use.evid2,facet=id,par.prof=NULL, debug = F){

    if(debug) browser()
    library(ggplot2)
    library(scales)

    ##    browser()
    
##### check arguments
    stopifnot(is.data.frame(data))
    stopifnot(x%in%colnames(data))
    ## if data does not contain an EVID column, it is assumed to be only observations
    if(!"EVID"%in%colnames(data)) x[,"EVID"] <- 0

    ## add reset info in separate column
    data$reset <- NA
    data$reset[data$EVID%in%c(3,4)] <- data[,x][data$EVID%in%c(3,4)]
    
    
### divide data into obs and sim (if EVID 2 present)
    if(missing(use.evid2)){
        use.evid2 <- !is.null(data[["EVID"]])&&c(2)%in%data[["EVID"]]
    }

    if(is.numeric(data[,par.prof])) data[,par.prof] <- as.factor(data[,par.prof])
    if(is.numeric(data[,grp])) data[,grp] <- as.factor(data[,grp])
    
########### plot settings ##############
    logbreaks <- c(outer(c(1,2,5),10^c(-10:10)))

    ## insert columns as labels if NA
    if(is.null(xlab)) xlab <- x
    if(is.null(ylab)) ylab <- dv
    if(is.null(ylab2)&&!is.null(amt)) ylab2 <- amt
    
    name.pred <- "Population prediction"
    name.ipred <- "Individual prediction"
    name.obs <- "Observations"
    name.lloq <- "LLOQ"

    ## maxes <- with(data,tapply(dv,list(grp),max,na.rm = T))
    DTdata <- data.table(data)
    DTdata[,s.dv.dos := max(get(dv),na.rm = T),by = grp]
    data <- as.data.frame(DTdata)
    
########### end plot settings ##############

    
    ## Loop each value of grp 
    outlist <- list()

    lvls <- unique(data[,grp])
    for(G in 1:length(lvls)){
## lapply(1:length(lvls),function(G){
        tmp <- data[data[,grp]==lvls[G],]
        tmp$IDnew <- as.numeric(as.factor(tmp[,id]))
        tmp$IDcut <- ((tmp$IDnew)-1) %/% NPerSheet + 1 
        
        ## Cut IDs in sheets
        maxcut <- max(tmp$IDcut)
        group = unique(tmp[,grp])
        
#### Set ranges.
### xrange should only depend on obs - not model
        ## xrange <- range(subset(tmp,EVID==0)$x,na.rm=T)
        xrange <- range(tmp[tmp$EVID==0,x],na.rm=T)
        if(!all(is.finite(xrange))) {
            warning(paste("Skipping one level:",lvls[G]))
            next
            ## {stop("range of independent variable cannot be determined for EVID==0")}
        }
        ## if(length(xlim) > 1) xrange <- xlim
        
### not used, and doesnt work. $ipred???
### ylim[1] should only depend on observations - not model
        ## yrange <- range(c(data$dv, data$ipred, data$pred), na.rm = T)
        ## we could get into trouble here if x is na
        if(any(is.na(tmp[tmp$EVID==2,x]))) stop("NA in simulated times. Very strange.")
        ## yrange <- range(subset(tmp,EVID==0|(EVID==2&x>=xrange[1]&x<=xrange[2]))$dv,na.rm=T)
        dat.yrange <- tmp[tmp$EVID==0|tmp$EVID==2&tmp[,x]>=xrange[1]&tmp[,x]<=xrange[2],]

#### LLOQ should be taken into account if provided
        yrange <- range(dat.yrange[,dv],na.rm=T)

        if(logy == T)  yrange <- range(dat.yrange[dat.yrange[,dv]>0,dv],na.rm=T)

        max.dv <- max(tmp[,dv],na.rm = T)
        max.dos <- max(tmp[,amt],na.rm = T)
        s.dv.dos <- max.dv/max.dos
        tmp$amt2 <- tmp[,amt]*tmp[,"s.dv.dos"]


        
        ## Loop through sheets (NPerSheet in each): Plot and save in list
        outlist.grp <- list()
        for(cut in 1:max(tmp$IDcut)){
            
            tmp2 <- subset(tmp, IDcut == cut)
            

            ptitle <- paste(run, "\n", group)
            if(maxcut>1) {
                ptitle <- paste(run, "\n", group, "\n", cut, "/", maxcut)
            }

            p <- NULL
            p <- ggplot(subset(tmp2,EVID==0), aes_string(x = x, y = dv))

            if(!is.null(amt)){
                p <- p+geom_segment(mapping = aes_string(x = x, xend = x, y = 0, yend = "amt2"),data=subset(tmp2,EVID%in%c(1,4)))
            }
            
            if(is.null(par.prof)){
                p <- p + geom_point(aes_(shape = name.obs))
            } else {
                p <- p + geom_point(aes_(shape = name.obs,colour=as.name(par.prof)))
            }
            if(use.evid2){
                
######not tested
                if(!is.null(ipred)){
                    p <- p +
                        geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof),linetype=ipred),data=subset(tmp2,EVID==2))
                }
                if(!is.null(pred)) {
                    p <- p+
                        geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof),linetype=pred),data=subset(tmp2,EVID==2))
                }
            } else {
                ## p <- p + geom_line(aes_(y = as.name(ipred), colour = ipred))
                ## p <- p + geom_line(aes_(y = as.name(pred) , colour = pred))
                if(is.null(par.prof)){
                    if(!is.null(ipred)){
                        p <- p + geom_line(aes_(y = as.name(ipred), linetype=ipred))
                    }
                    if(!is.null(pred)){
                        p <- p + geom_line(aes_(y = as.name(pred) , linetype=pred))
                    }
                } else {
                    if(!is.null(ipred)){
                        p <- p + geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof),linetype=ipred))
                    }
                    if(!is.null(pred)){
                        p <- p + geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof),linetype=pred))
                    }
                }
            }

            if(logy){
                ## browser()
                ## if(!is.null(amt)){
                ##     scale_y_log10(breaks = logbreaks,label=comma,
                ##                   sec.axis = sec_axis(~./s.dv.dos, name = ylab2))
                ## } else {
                    p <- p + scale_y_log10(breaks = logbreaks,label=comma)

                ## }
            } ##else {

##                if(!is.null(amt)){
                    
  ##                  p <- p + scale_y_continuous(sec.axis = sec_axis(~./get(s.dv.dos), name = ylab2))
##                    cat("just added scale",s.dv.dos)
    ###            }
                ## }
            
            p <- p + labs(title = ptitle, x = xlab, y = ylab, colour = "fit")
            
            if(!free.lims) {
                p <- p + coord_cartesian(xlim = xrange, ylim = yrange)
            }
            ## browser()
            if(!is.null(LLOQ)){
                p <- p+geom_hline(aes_(yintercept=as.name(LLOQ),linetype=name.lloq))
                p <- p+scale_linetype_manual(values=2)
            }
            
            if (!is.null(facet)){
                
                p <- p + facet_wrap(reformulate(facet), ncol = 3)
            }
            ##            p <- p + scale_colour_manual(values = c("red", "black"))
            p <- p + theme(legend.position = "bottom",legend.title=element_blank(),legend.box="horizontal")
             outlist.grp <- c(outlist.grp, list(p))
            cat(paste(paste(group, " ", cut, "/", maxcut, sep = ""), "created.\n" ))
            ##            cat("s.dv.dos is",s.dv.dos,"\n")
            ## p
        }
        
        
        outlist <- c(outlist,outlist.grp)
    }
    
    return(outlist)
}

