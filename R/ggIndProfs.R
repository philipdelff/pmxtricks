##' Plot individual profiles and doses based on NM style dataset
##' 
##' @param data The dataset to plot.
##' @param run The main title of the plot. Called run becaus you often
##'     want a Nonmem run name here.
##' @param x The name of the column to be plotted on the x-axis
##'     (string).
##' @param dv The name of the column containing observations (string).
##' @param pred The name of the population predictions in data
##'     (string).
##' @param ipred The name of the individual predictions in data
##'     (string).
##' @param grp A grouping variable. Subjects will be split into groups
##'     of common values (string).
##' @param amt The name of the column containing dose amounts. If this
##'     argument is given, the doses will be included as vertical bars
##'     in the plots, with a secondary axis representing the dose
##'     amounts on the right.
##' @param id The name of the subject ID column
##' @param use.evid2 Should EVID 2 records be used for pred and ipred
##'     plotting? The default is to use EVID==2 records if data
##'     contains a column called EVID, and this column has at least
##'     one value equalling 2.
##' @param facet splits plots in pages
##' @param par.prof Distinguish multiple profiles in dataset.
##' @param xlab label for x-axis.
##' @param ylab label for y-axis.
##' @param ylab2 label for y-axis to the right representing dose
##'     amounts.
##' @param scales passed to facet_wrap.
##' @param logy Show y-axis on logarithmic scale?
##' @param NPerSheet Number of subjects per sheet
##' @param LLOQ Lower limiit of quantification (will be shown as a
##'     horizontal line in plots).
##' @param x.inc Values that must be included in the span of the
##'     x-axis. This can be multiple values, like c(5,1000).
##' @param grp.label Column to use for labeling the sheets (while
##'     sorting by grp). A typical example is that grp is numeric (say
##'     dose including 80 and 280) while grp.label is a character
##'     (including 80 mg and 280 mg). In order to sort correctly, you
##'     must use the numeric variable for grp. But in order to get
##'     nice labels, use the character variable for labels.
##' @param labels The default is to include the subject id's in labels
##'     above the plots (using facet_wrap()). Set this to FALSE to
##'     remove these labels in order to have more space for the plots
##'     themselves.
##' @param debug Start by calling debug()?
##' @param debug.sheet If something goes wrong when plotting, this may
##'     be the debug method to use. Pass an integer to call browser()
##'     when creating the corresponding sheet.
##' @details The resulting plot object has been limited on x axis by
##'     coord_cartesian. So if you want to adjust x limits on the
##'     output from this function, you must use coord_cartesian. xlim
##'     does not work.
##'
##' The resulting object can be saved with ggwrite.
##' @import ggplot2
##' @import scales
##' @import data.table
##' @importFrom stats reformulate
##' @family Plotting

##' @export


### change log
## 2019-04-01 philipdelff: Added support for plotting of doses.

## 2019-03-28 philipdelff: the two for loops to control faceting is
## horribly inflexible. Do the plot, then facet. First: for splitting
## into pages, introduce a variable (column) that groups into the
## splitting needed.

### end change log

#### todo

## 2019-04-09 philipdelff: doses should be added by x before plotting

## 2019-04-02 philipdelff: scale ranges. Fix within grp, sheet, or
## totally free.

## 2019-04-02 philipdelff: x and y scales should be controlled individually.

## 2019-04-02 philipdelff: Include possibility of limiting scale by
## pk, dose, and/or predictions

## This function performs two different operations. It plots profiles,
## and it splits the profiles by faceting and by splitting into a list
## of plots. At least the last part of splitting into a list should be
## split into a separate function.

## only plot available variables. Warn users what not found if verbose?

## there must be something easier than the format data stuff 

## structure the evid selection

## optionally colour all by id

## In yrange LLOQ should be taken into account if provided

### end todo



ggIndProfs <- function(data, run, x="TIME", dv="DV", pred="PRED", ipred=c("IPRED","IPRE"), grp, amt , id = "ID", xlab = NULL, ylab = NULL, ylab2 = NULL, scales = "fixed", logy = F, NPerSheet=12,LLOQ=NULL, use.evid2, facet=id, par.prof=NULL, x.inc,grp.label = grp, labels=TRUE, debug = F, debug.sheet){
    if(debug) browser()


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    EVID <- NULL
    reset <- NULL
    ..par.prof <- NULL
    .amt <- NULL
    s.dv.dos <- NULL
    amt2 <- NULL
    IDnew <- NULL
    IDcut <- NULL
    grp.char <- NULL
    sheet <- NULL
    sheetgrp <- NULL
    Nsheetsgrp <- NULL
    xmingrp <- NULL
    xmaxgrp <- NULL
    skipgrp <- NULL
    ymingrp <- NULL
    ymaxgrp <- NULL
    val <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
##### check arguments
    if(!is.data.frame(data)) stop("data has to be a data.frame.")
    if(!x%in%colnames(data)) {stop(paste(x,"not found in data (see x argument)."))}
    ## if data does not contain an EVID column, it is assumed to be only observations
    if(!"EVID"%in%colnames(data)) x[,"EVID"] <- 0

    if(!is.null(dv)) {
        if(!dv%in%names(data)) stop("dv not found in data.")
### as long as we haven't implemented skipping plotting of dv we need this.
        if(all(is.na(as.data.frame(data)[,dv]))) stop("dv column in data contains no observations. This is not supported.")
    }


    if(!is.null(pred)) {
        if(length(pred)==0) pred <- NULL 
        found.pred <- pred %in% names(data)
        if(sum(found.pred)==0) {
            message("Population predictions not found in data. Skipping.")
            pred <- NULL
        } else {
            pred <- pred[found.pred] [1]
        }
        
    }
    
    if(!is.null(ipred)) {
        if(length(ipred)==0) ipred <- NULL 
        found.ipred <- ipred %in% names(data)
        if(sum(found.ipred)==0) {
            message("Individual predictions not found in data. Skipping.")
            ipred <- NULL
        } else {
            ipred <- ipred[found.ipred] [1]
        }
        
    }

    if(missing(amt)) amt <- NULL
    
    if(missing(grp)) {
        data$..grp <- ""
        grp <- "..grp"
    }
    
### do all 

    ## helper function. Get a default value if data is empty or na only.
    na.or.eval <- function(fun,x,default=NA_real_,...) {
        if(length(x)==0||length(x[is.finite(x)])==0) out <- default
        else out <- fun(x,...)
        out
    }


    DTdata <- data.table(data)
    
    ## add reset info in separate column
    DTdata$reset <- NA
    ##    data$reset[data$EVID%in%c(3,4)] <- data[,get(x)][data$EVID%in%c(3,4)]
    DTdata[EVID%in%c(3,4),reset:=get(x)]
    
    
### divide data into obs and sim (if EVID 2 present)
    if(missing(use.evid2)){
        use.evid2 <- !is.null(data[["EVID"]])&&c(2)%in%data[["EVID"]]
    }
    if(missing(run)){
        run <- ""
    }

    if(is.null(par.prof)) {
        par.prof <- "..par.prof"
        DTdata[,..par.prof:="profile"]
    }
    if(is.numeric(DTdata[,get(par.prof)])) DTdata[,c(par.prof):=list(as.factor(get(par.prof)))]
    ##        DTdata[,get(par.prof):=as.factor(get(par.prof))]
    
    ## if(is.numeric(data[,grp])) data[,grp] <- as.factor(data[,grp])
    
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
    name.dos <- "Doses"

    ## maxes <- with(data,tapply(dv,list(grp),max,na.rm = T))
    
    if(is.null(amt)) {
        plot.doses <- FALSE
        amt <- ".amt"
        ## just a dummy, for calcs not to fail. Will not be used in plots.
        DTdata[,.amt := 1]
    } else {
        plot.doses <- TRUE
    }
    ##    DTdata[,s.dv.dos := max(get(dv),na.rm = T)/max(get(amt),na.rm = T),
    ##           by = get(grp)]
    
    ## vartmp <- "s.dv.dos"
    

    ## DV is ignored by nonmem if EVID != 0. We do the same.
    DTdata[EVID!=0,(dv):=NA]
    
    DTdata[, s.dv.dos:=na.or.eval(max,get(dv),default=1,na.rm = T)/max(get(amt),na.rm = T), by = grp]
    DTdata[,amt2:=get(amt)*s.dv.dos,by=grp]

    DTdata[,
           IDnew := as.numeric(as.factor(get(id))),
           by = grp]

    DTdata[,
           IDcut := ((IDnew)-1) %/% NPerSheet + 1 ,
           by = grp]
    DTdata[,grp.char:=get(grp)]
    
    DTdata$..ROW <- 1:nrow(DTdata)
    ##    DTdata[with(DTdata,order(get(grp),IDcut,..ROW)),]
    setorderv(DTdata,cols = c(grp,"IDcut","..ROW"))
    
    DTdata[,sheet:= .GRP,c(grp,"IDcut")]

    ## add a counter to sheets within grp level - and no of sheets within grp level
    DTdata[,sheetgrp := as.numeric(as.factor(sheet)),grp]
    DTdata[,Nsheetsgrp := max(sheetgrp),grp]

### Making sure that values of x.inc will be included on th x.axis.
    DTdata[,xmingrp :=  NA_real_]
    DTdata[,xmaxgrp :=  NA_real_]
    if(!missing(x.inc)) DTdata[,xmingrp := min(x.inc)] 
    if(!missing(x.inc)) DTdata[,xmaxgrp := max(x.inc)]
    
    ## DTdata[,xmingrp := min(c(get(x)[EVID == 0],xmingrp),na.rm=T),grp]
    ## DTdata[,xmaxgrp := max(c(get(x)[EVID == 0],xmaxgrp),na.rm=T),grp]

    DTdata[,xmingrp := na.or.eval(min,c(get(x)[EVID == 0],xmingrp),na.rm=T),grp]
    DTdata[,xmaxgrp := na.or.eval(max,c(get(x)[EVID == 0],xmaxgrp),na.rm=T),grp]
    ## subset.xrange <- "EVID == 0"
    ## DTdata[,xmingrp := min(c(get(x)[eval(parse(text = subset.xrange))],xmingrp),na.rm=T),get(grp)]
    ## DTdata[,xmaxgrp := max(c(get(x)[eval(parse(text = subset.xrange))],xmaxgrp),na.rm=T),get(grp)]

    ##    browser()
### by=get(grp) was changed to by=eval(grp) because the former didnt work with only one patient.
    ##     DTdata[,skipgrp := any(!is.finite(xmingrp)|!is.finite(xmaxgrp)),get(grp)]
    DTdata[,skipgrp := any(!is.finite(xmingrp)|!is.finite(xmaxgrp)),by=eval(grp)]

    if(nrow(DTdata[isTRUE(skipgrp)])>0){
        
        warning("These group levels have no valid observations. Skipping:",
                paste(DTdata[isTRUE(skipgrp),unique(get(grp))])
                )
    }
    
    ## DTdata[,ymingrp := min(get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp],na.rm=T),grp]
    ## DTdata[,ymaxgrp := max(get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp],na.rm=T),grp]
    DTdata[,ymingrp := na.or.eval(min,get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp],na.rm=T),grp]
    DTdata[,ymaxgrp := na.or.eval(max,get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp],na.rm=T),grp]
    
    if(logy == T){
        ## DTdata[,ymingrp := min(get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp&get(x)>0],na.rm=T),get(grp)]
        ## DTdata[,ymaxgrp := max(get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp&get(x)>0],na.rm=T),get(grp)]
        DTdata[,ymingrp := na.or.eval(min,get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp&get(x)>0],na.rm=T),get(grp)]
        DTdata[,ymaxgrp := na.or.eval(max,get(dv)[EVID == 0|EVID==2&get(x)>=xmingrp&get(x)<=xmaxgrp&get(x)>0],na.rm=T),get(grp)]
    }


    ## max.dv <- max(tmp[,dv],na.rm = T)
    ## max.dos <- max(tmp[,amt],na.rm = T)
    ## s.dv.dos <- max.dv/max.dos


    
    ##    xrange <- rangetmp[tmp$EVID==0,x],na.rm=T)
    
    data <- as.data.frame(DTdata)

### need to order at this point - to get plots in correct order
    data$..ROW <- 1:nrow(data)
    data <- data[with(data,order(sheet,..ROW)),]

    
    
########### end plot settings ##############


    if(any(is.na(data[data$EVID==2&!data$skipgrp,x]))) stop("NA in simulated times. Very strange.")
    
    ## Loop each value of grp 
    ## outlist <- list()

    ##    lvls <- unique(data[,grp])
    ## for(G in 1:length(lvls)){
    
    ## lapply(1:length(lvls),function(G){
    
    ##    n.plots <- 0
    if(missing(debug.sheet)) debug.sheet <- NULL
    outlist <- by(data,data$sheet,ds=debug.sheet,FUN=function(tmp,ds){##        tmp <- data[data[,grp]==lvls[G],]
        if(!is.null(ds) && debug.sheet==unique(tmp$sheet)) browser()
        ##        tmp$IDnew <- as.numeric(as.factor(tmp[,id]))
        ##        tmp$IDcut <- ((tmp$IDnew)-1) %/% NPerSheet + 1 
        
        
#### Set ranges.
### xrange should only depend on obs - not model
        ## xrange <- range(tmp[tmp$EVID==0,x],na.rm=T)
        ## if(!all(is.finite(xrange))) {
        ##     warning(paste("Skipping one level:",lvls[G]))
        ##     next
        ##     ## {stop("range of independent variable cannot be determined for EVID==0")}
        ## }
        ## if(length(xlim) > 1) xrange <- xlim
        
### not used, and doesnt work. $ipred???
### ylim[1] should only depend on observations - not model
        ## yrange <- range(c(data$dv, data$ipred, data$pred), na.rm = T)
        ## we could get into trouble here if x is na
        ## if(any(is.na(tmp[tmp$EVID==2,x]))) stop("NA in simulated times. Very strange.")
        ## yrange <- range(subset(tmp,EVID==0|(EVID==2&x>=xrange[1]&x<=xrange[2]))$dv,na.rm=T)
        ## dat.yrange <- tmp[tmp$EVID==0|tmp$EVID==2&tmp[,x]>=xrange[1]&tmp[,x]<=xrange[2],]

#### LLOQ should be taken into account if provided
        ## yrange <- range(dat.yrange[,dv],na.rm=T)
        ## if(logy == T)  yrange <- range(dat.yrange[dat.yrange[,dv]>0,dv],na.rm=T)


        ## max.dv <- max(tmp[,dv],na.rm = T)
        ## max.dos <- max(tmp[,amt],na.rm = T)
        ## s.dv.dos <- max.dv/max.dos
        ## tmp$amt2 <- tmp[,amt]*tmp[,"s.dv.dos"]

        
        ## Loop through sheets (NPerSheet in each): Plot and save in list
        ## outlist.grp <- list()
        ## for(cut in 1:max(tmp$IDcut)){

        ## tmp2 <- subset(tmp, IDcut == cut)
        tmp2 <- tmp
        group <- unique(tmp[,grp])
        xrange <- c(unique(tmp$xmingrp),unique(tmp$xmaxgrp))
        yrange <- c(unique(tmp$ymingrp),unique(tmp$ymaxgrp))
        s.dv.dos <- unique(tmp[,"s.dv.dos"])

        
        ptitle <- ""
        if(run != "") ptitle <- paste0(run,". ")
        ## ptitle <- paste0(ptitle," ",paste(c(
        ##                                 unique(tmp[,grp.label])
        ##                             ),collapse=", "),".")
        ## df.tmp <- unique(tmp[,grp.label,drop=F])

        if(length(grp)>1 || grp!="..grp"){
            ltmp <- lapply(unique(tmp[,grp.label,drop=F]),as.character)
            tmpnames = data.frame(var=names(ltmp),val=c(sapply(ltmp,identity)))
            ptitle <- paste0(ptitle,paste(within(tmpnames,{yo=paste(var,val,sep="=")})$yo,collapse=", "),".")
        }
        
        if(unique(tmp$Nsheetsgrp)>1) {
            ptitle <- paste0(ptitle, " ",unique(tmp$sheetgrp), "/", unique(tmp$Nsheetsgrp),".")
        }

        ## if(unique(tmp2$sheet)==30) browser()
        p <- NULL
        ## if(unique(tmp2$sheet)==30) browser()
        p <- ggplot(tmp2, aes_string(x = x, y = dv))

        if(plot.doses){
            
            ## if(is.null(par.prof)){
            p <- p+geom_segment(mapping = aes_string(x = x, xend = x, y = 0, yend = "amt2",colour=par.prof),data=subset(tmp2,EVID%in%c(1,4)))
            ## } else {
            ##     p <- p+geom_segment(mapping = aes_string(x = x, xend = x, y = 0, yend = "amt2",colour=as.name(par.prof)),data=subset(tmp2,EVID%in%c(1,4)))
            ## }
        }

        
        
### I can't get a single call to work independently of whether par.prof is given or not.
        ##  p <- p + geom_point(aes_(shape = as.name(name.obs), colour = as.name(par.prof)))
        if(nrow(subset(tmp2,EVID==0))>0){
            if(is.null(par.prof)){
                p <- p + geom_point(aes_(shape = name.obs))
            } else {
                ## p <- p + geom_point(aes_(shape = name.obs))
                ## p <- p + geom_point(aes_string(shape = name.obs, colour = par.prof))
                p <- p + geom_point(aes_(shape = eval(name.obs), colour = as.name(par.prof)))
            }
        }

        if(use.evid2){
            

            if(!is.null(ipred)){
                p <- p +
                    geom_line(aes_(y = as.name(ipred), colour = as.name(par.prof),linetype=ipred),data=subset(tmp2,EVID==2))
            }
            if(!is.null(pred)) {
                p <- p+
                    geom_line(aes_(y = as.name(pred) , colour = as.name(par.prof),linetype=pred),data=subset(tmp2,EVID==2))
            }
        } else {
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
            if(plot.doses){
                p <- p + scale_y_log10(breaks = logbreaks, label=comma,
                                       sec.axis = sec_axis(~./s.dv.dos, name = ylab2,labels=comma))
                
            } else {
                p <- p + scale_y_log10(breaks = logbreaks,label=comma)
            }
            ## }
        } else {

            if(plot.doses){
                ##      if(unique(tmp2$sheet)==30) browser()
                p <- p + scale_y_continuous(sec.axis = sec_axis(~./s.dv.dos, name = ylab2))
            }
        }
        
        p <- p + labs(title = ptitle, x = xlab, y = ylab, colour = "fit")
        
        ## browser()
        if(!is.null(LLOQ)){
            p <- p+geom_hline(aes_(yintercept=as.name(LLOQ),linetype=name.lloq))
            p <- p+scale_linetype_manual(values=2)
        }
        
        if (!is.null(facet)){
            p <- p + facet_wrap(reformulate(facet), ncol = 3,scales = scales)
        }
        ##            p <- p + scale_colour_manual(values = c("red", "black"))
        p <- p + theme(legend.position = "bottom",legend.title=element_blank(),legend.box="horizontal")
        ##        outlist.grp <- c(outlist.grp, list(p))
        ## not done yet - replacce unique(12) by Noofsheets

        ## if(!scales%in%c("free_x","free")){
        ##     p <- p + coord_cartesian(xlim = xrange)
        ## }
        ## if(!scales%in%c("free_y","free")){
        ##     p <- p + coord_cartesian(ylim = yrange)
        ## }

        
        xlim <- NULL
        if(scales%in%c("free","free_x")||any(is.na(xrange))) xlim <- NULL else xlim <- xrange
        if(scales%in%c("free","free_y")||any(is.na(xrange))) ylim <- NULL else ylim <- yrange

        if(!scales%in%c("free")){
            p <- p + coord_cartesian(
                         xlim = xlim,
                         ylim = ylim
                     )
        }

        ##        n.plots <- n.plots+1 cat(paste0(paste(n.plots,": ",
        ##   unique(tmp$sheetgrp), "/", unique(tmp$Nsheetsgrp), sep = ""),
        ##   "created.\n" ))
        ##        if(unique(tmp2$sheet)==30) browser()
        message(paste0(unique(tmp2$sheet),": ", ptitle, " created." ))
        ##            cat("s.dv.dos is",s.dv.dos,"\n")

        if(!labels){
            p <- p+theme(
                       strip.background = element_blank(),
                       strip.text.x = element_blank())
        }
        p
        
    }
    
    
    ##  outlist <- c(outlist,outlist.grp)
    )
    
    return(outlist)
}

