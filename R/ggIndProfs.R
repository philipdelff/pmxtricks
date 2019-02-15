##### INDIVIDUAL PLOTS IN LIST

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
ggIndProf <- function(data, run = "", x="TIME", dv="DV", pred="PRED", ipred="IPRE", grp = "GRP", id = "ID", xlab = NA, ylab = NA, free.lims = F, logy = F, NPerSheet=12,LLOQ=NULL, use.evid2,facet=id,par.prof=NULL, debug = F){

    if(debug) browser()
    library(ggplot2)
    library(scales)

##    browser()
    
##### check arguments
    stopifnot(is.data.frame(data))
    stopifnot(x%in%colnames(data))
    ## if data does not contain an EVID column, it is assumed to be only observations
    if(!"EVID"%in%colnames(data)) x[,"EVID"] <- 0
    
        
### divide data into obs and sim (if EVID 2 present)
    if(missing(use.evid2)){
        use.evid2 <- !is.null(data[["EVID"]])&&c(2)%in%data[["EVID"]]
    }

    if(is.numeric(data[,par.prof])) data[,par.prof] <- as.factor(data[,par.prof])
    if(is.numeric(data[,grp])) data[,grp] <- as.factor(data[,grp])
    
########### plot settings ##############
    logbreaks <- c(outer(c(1,2,5),10^c(-10:10)))

    ## insert columns as labels if NA
    if(is.na(xlab)) xlab <- x
    if(is.na(ylab)) ylab <- dv

    name.pred <- "Population prediction"
    name.ipred <- "Individual prediction"
    name.obs <- "Observations"
    name.lloq <- "LLOQ"
    
########### end plot settings ##############


    
    ## Loop each group
    outlist <- list()

    lvls <- unique(data[,grp])
    for(G in 1:length(lvls)){

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
        ## if(logy == T) yrange <- range(c(data$dv[data$dv>0], data$ipred[data$ipred>0], data$pred[data$pred>0]), na.rm = T)
        
        
        ## Loop through sheets (NPerSheet in each): Plot and save in list
        outlist.grp <- list()
        for(cut in 1:max(tmp$IDcut)){
## browser()
            
            tmp2 <- subset(tmp, IDcut == cut)
            ptitle <- paste(run, "\n", group)
            if(maxcut>1) {
                ptitle <- paste(run, "\n", group, "\n", cut, "/", maxcut)
            }

            p <- NULL
            p <- ggplot(subset(tmp2,EVID==0), aes_string(x = x, y = dv))

            if(is.null(par.prof)){
                p <- p + geom_point(aes_(shape = name.obs))
            } else {
                p <- p + geom_point(aes_(shape = name.obs,colour=as.name(par.prof)))
            }
            if(use.evid2){
                p <- p +
                    ## geom_line(aes(y = col_ipred, colour = ipred),data=subset(tmp2,EVID==2))+
                    ## geom_line(aes_(y = as.name(ipred), colour = name.ipred),data=subset(tmp2,EVID==2))+
                    ## geom_line(aes_(y = as.name(pred) , colour = name.pred ),data=subset(tmp2,EVID==2))
######not tested
    geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof),linetype=ipred),data=subset(tmp2,EVID==2))+
    geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof),linetype=pred),data=subset(tmp2,EVID==2))
            } else {
                ## p <- p + geom_line(aes_(y = as.name(ipred), colour = ipred))
                ## p <- p + geom_line(aes_(y = as.name(pred) , colour = pred))
                if(is.null(par.prof)){
                    p <- p + geom_line(aes_(y = as.name(ipred), linetype=ipred))
                    p <- p + geom_line(aes_(y = as.name(pred) , linetype=pred))
                } else {
                    p <- p + geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof),linetype=ipred))
                    p <- p + geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof),linetype=pred))
                }
            }
            if(logy){
                ## browser()
                p <- p + scale_y_log10(breaks = logbreaks,label=comma)
            }
            
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
            cat(paste(paste(group, " ", cut, "/", maxcut, sep = ""), "created\n"))
        }
        outlist <- c(outlist,outlist.grp)
    }
    
    return(outlist)
}

### ggIndProf.old is a backup before implementing multiple profiles per ID
ggIndProf.old <- function(data, run = "", x="TIME", dv="DV", pred="PRED", ipred="IPRE", grp = "GRP", id = "ID", xlab = NA, ylab = NA, free.lims = F, logy = F, LLOQ=NULL, use.evid2,facet=id,par.prof=NULL, debug = F){

    ## browser()
    
    
    if(debug) browser()

#######  Experimental   #########
### create integer group numbers
    ## tmpGrp <- data.frame(grp = unique(data$grp))
    ## tmpGrp <- arrange(tmpGrp, grp)
    ## tmpGrp$tempGRP <- 1:length(unique(data$grp))
    ## data <- merge(data, tmpGrp)
### This is done much more easily with factors. But in fact not needed at all.
##    data$tempGRP <- as.numeric(factor(data[,grp]))
        
### divide data into obs and sim (if EVID 2 present)
    if(missing(use.evid2)){
        use.evid2 <- !is.null(data[["EVID"]])&&c(2)%in%data[["EVID"]]
    }

    if(is.null(par.prof)) {
        par.prof <- "par.prof"
        data[,par.prof] <- ""
    }
    
########### plot settings ##############
    logbreaks <- c(outer(c(1,2,5),10^c(-10:10)))

    ## insert columns as labels if NA
    if(is.na(xlab)) xlab <- x
    if(is.na(ylab)) ylab <- dv

    name.pred <- "Population prediction"
    name.ipred <- "Individual prediction"
    name.obs <- "Observations"
    name.lloq <- "LLOQ"
    
########### end plot settings ##############


    
    ## Loop each group
    outlist <- list()

    lvls <- unique(data[,grp])
    for(G in 1:length(lvls)){

        tmp <- data[data[,grp]==lvls[G],]
        tmp$IDnew <- as.numeric(as.factor(tmp[,id]))
        tmp$IDcut12 <- ((tmp$IDnew)-1) %/% 12 + 1 
    
        ## Cut in ID groups of 12
        maxcut <- max(tmp$IDcut12)
        group = unique(tmp[,grp])
        
#### Set ranges.
### xrange should only depend on obs - not model
        ## xrange <- range(subset(tmp,EVID==0)$x,na.rm=T)
        xrange <- range(tmp[tmp$EVID==0,x],na.rm=T)
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
        ## if(logy == T) yrange <- range(c(data$dv[data$dv>0], data$ipred[data$ipred>0], data$pred[data$pred>0]), na.rm = T)
        
        
        ## Loop each IDgroup (12 in each): Plot and save in list
        outlist.grp <- list()
        for(cut in 1:max(tmp$IDcut12)){
## browser()
            
            tmp2 <- subset(tmp, IDcut12 == cut)
            ptitle <- paste(run, "\n", group)
            if(maxcut>1) {
                ptitle <- paste(run, "\n", group, "\n", cut, "/", maxcut)
            }

            p <- NULL
            p <- ggplot(subset(tmp2,EVID==0), aes_string(x = x, y = dv)) 
            p <- p + geom_point(aes(shape = name.obs))
            if(use.evid2){
                p <- p +
                    ## geom_line(aes(y = col_ipred, colour = ipred),data=subset(tmp2,EVID==2))+
                    ## geom_line(aes_(y = as.name(ipred), colour = name.ipred),data=subset(tmp2,EVID==2))+
                    ## geom_line(aes_(y = as.name(pred) , colour = name.pred ),data=subset(tmp2,EVID==2))
######not tested
    geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof)),data=subset(tmp2,EVID==2))+
    geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof)),data=subset(tmp2,EVID==2))
            } else {
                ## p <- p + geom_line(aes_(y = as.name(ipred), colour = ipred))
                ## p <- p + geom_line(aes_(y = as.name(pred) , colour = pred))
                if(is.null(par.prof)){
                    p <- p + geom_line(aes_(y = as.name(ipred),linetype=ipred))
                    p <- p + geom_line(aes_(y = as.name(pred) ,linetype=pred))
                } else {
                    p <- p + geom_line(aes_(y = as.name(ipred), colour = par.prof,linetype=ipred))
                    p <- p + geom_line(aes_(y = as.name(pred) , colour = par.prof,linetype=pred))
                }
            }
            if(logy){
                ## browser()
                p <- p + scale_y_log10(breaks = logbreaks,label=comma)
            }
            
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
            ## p <- p + scale_colour_manual(values = c("red", "black"))
            ## p <- p+scale_colour_continuous(guide=F)
            p <- p + theme(legend.position = "bottom",legend.title=element_blank(),legend.box="horizontal")

            
            outlist.grp <- c(outlist.grp, list(p))
            cat(paste(paste(group, " ", cut, "/", maxcut, sep = ""), "created\n"))
        }
        outlist <- c(outlist,outlist.grp)
    }
    
    return(outlist)
}



ggQCP_ind_pd <- function(...){
warning("ggQCP_ind_pd deprecated. Use ggIndProf.")
    ggIndProf(...)
}
