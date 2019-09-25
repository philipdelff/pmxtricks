##' Get one plot of observations, predictions, and means vs time
##' @param data
##' @param geom.dv
##' @param geom.pred
##' @param geom.ipred
##' @param geom.dvmean
##' @param geom.predmean
##' @param geom.ipredmean
##' @param grp
##' @param type.mean
##' @param col.time
##' @param col.ntim
##' @param col.dv
##' @param col.ipred
##' @param col.pred
##' @param debug
##' @import data.table
##' @import ggplot2
##' @examples
##' library(pmxtricks)
##' data(pksim1,package="pmxtricks")
##' pksim2 <- copy(pksim1)
##' pksim2[,PRED:=DV+rnorm(.N)*10]
##' pksim2[,NOMTIME:=TIME]
##' pksim2[,TIME:=round(TIME+rnorm(.N),2)]
##' 
##' 
##' NMplotFit2(pksim2,
##'            geom.dv = "p",
##'            geom.pred = "l",
##'            geom.dvmean = "lc",
##'            geom.predmean = "p",
##'            grp=NULL,
##'            type.mean = "arithmetic",
##'            col.ntim = "NOMTIME",
##'            col.dv = "DV",
##'            col.ipred = NULL,
##'            col.pred = "PRED",
##'            debug=F)+
##'     theme_pp()
##' 

####### todo

### check validity of geom.dv etc

####### end todo 


NMplotFit <- function(data, 
                       geom.dv= "p",
                       geom.pred = "",
                       geom.ipred = "",
                       geom.dvmean = "pc",
                       geom.predmean = "l",
                       geom.ipredmean="",
                       grp=NULL,
                       type.mean = "arithmetic",
                       col.time="TIME",
                       col.ntim = "NOMTIME",
                       col.dv = "DV",
                       col.ipred = NULL,
                       col.pred = "PRED",
                       debug=F){
    
    if(debug) browser()
    
    pksim2 <- copy(as.data.table(data))

    ## mvars <- c("DV","PRED","IPRED")[c("DV","PRED","IPRED")%in%colnames(pksim2)]
    mvars <- c(col.dv,col.pred,col.ipred)
    pklong <- melt(pksim2,id.vars=c("ID","ROW",col.time,col.ntim),measure.vars=mvars)
    ## ggplot(pklong)+geom_line(aes(TIME,value,linetype=variable,colour=ID))
    ## this one obs has points, pred has lines. melting is no good. Lines don't work. Or at least we have to subset to only get pred. I not we get DV in line legend even if there is no line for DV.


### adding means

    pklong3 <- copy(pklong)
    ##    pklong3[,mean.grp:=means(value),by=c(grp,col.ntim)]
    ## pklong3[,c("mean.grp","mean.grp.l","mean.grp.u"):=means(value,ci=T,type=type.mean),by=c(grp,col.ntim)]
    pklong3
    ## pklong4 <- melt(pklong3,id.vars=c("ID","ROW","TIME","NOMTIME","variable"),measure.vars=c("value"),variable.name="type")
    ## pklong4
    
    pklong3[,c("mean.grp","mean.grp.l","mean.grp.u"):=means(value,ci=T,type=type.mean),by=c(grp,col.ntim,"variable")]
    pklong3
    ## pklong3[NOMTIME==168]
    
    pklong4 <- melt(pklong3,id.vars=c("ID","ROW","TIME","NOMTIME","variable","mean.grp.l","mean.grp.u"),measure.vars=c("value","mean.grp"),variable.name="type")

    ##     pklong4[type%in%c("mean.grp","mean.grp.l","mean.grp.u"),ID:="mean"]
    pklong4[type%in%c("mean.grp"),ID:="mean"]
    pklong4 <- unique(pklong4,by=c("ID","NOMTIME","variable","value"))
    ## vars.ci <- c("mean.grp.l","mean.grp.u")
    ## pklong4[ID!="mean",(vars.ci):=NA]


    prepare.geoms <- function(geom,var234,type234){
        ##    browser()

        if(grepl("l",geom)){
            ## pklong4[variable==get("variable", envir = parent.frame())&
            ##         type==get("type", envir = parent.frame()),val.line:=value]
            pklong4[variable==var234&
                    type==type234,val.line:=value]
        }

        if(grepl("p",geom)){
            ## browser()
            ## pklong4[variable==..(variable)&type==..(type),val.point:=value]
            pklong4[variable==var234&
                    type==type234,val.point:=value]
        }

        if(grepl("c",geom)){
            ## pklong4[variable==get("variable", envir = parent.frame())&
            ##         type==get("type",envir = parent.frame()),val.ci.l:=mean.grp.l]
            pklong4[variable==var234&
                    type==type234,val.ci.l:=mean.grp.l]
            ## pklong4[variable==get("variable", envir = parent.frame())&
            ##         type==get("type",envir = parent.frame()),val.ci.u:=mean.grp.u]
            pklong4[variable==var234&
                    type==type234,val.ci.u:=mean.grp.u]
        }
        pklong4
    }

    prepare.geoms(geom.dvmean,var234="DV",type234="mean.grp")
    prepare.geoms(geom.predmean,var234="PRED",type234="mean.grp")
    prepare.geoms(geom.ipredmean,var234="IPRED",type234="mean.grp")
    prepare.geoms(geom.dv,var234="DV",type234="value")
    prepare.geoms(geom.pred,var234="PRED",type234="value")
    prepare.geoms(geom.ipred,var234="IPRED",type234="value")

    
    ## if(grepl("l",geom.predmean)){
    ##     pklong4[variable=="PRED"&type=="mean.grp",val.line:=value]
    ## }
    ## if(grepl("l",geom.dvmean)){
    ##     pklong4[variable=="DV"&type=="mean.grp",val.line:=value]
    ## }
    ## if(grepl("p",geom.predmean)){
    ##     pklong4[variable=="PRED"&type=="mean.grp",val.point:=value]
    ## }
    ## if(grepl("p",geom.dvmean)){
    ##     pklong4[variable=="DV"&type=="mean.grp",val.point:=value]
    ## }
    ## if(grepl("p",geom.pred)){
    ##     pklong4[variable=="PRED"&type=="value",val.point:=value]
    ## }
    ## if(grepl("l",geom.pred)){
    ##     pklong4[variable=="PRED"&type=="value",val.line:=value]
    ## }
    ## if(grepl("p",geom.dv)){
    ##     pklong4[variable=="DV"&type=="value",val.point:=value]
    ## }
    ## if(grepl("l",geom.dv)){
    ##     pklong4[variable=="DV"&type=="value",val.line:=value]
    ## }

    if(F){
        ## the ones to include in the scale
        pklong4[!is.na(val.line),unique(variable)]

        ## all type values
        levels(pklong4[,unique(type)])
        ## the ones to include in the scale
        pklong4[!is.na(val.line),unique(type)]
        ## the factor orders of these values
        Nval.fac.type <- as.numeric(pklong4[!is.na(val.line),unique(type)])
        ## the names of these values
        names.levels.type <- as.character(pklong4[!is.na(val.line),unique(type)])
        ## the number of factor levels
        n.levels.type <- length(levels(pklong4[,unique(type)]))
        values.fac.type <- rep(NA_integer_,n.levels.type)
        values.fac.type[Nval.fac.type] <- 1:length(names.levels.type)
    }

    
    genScaleLevels <- function(col.val,col.var,values){
        ## browser()
        ## all type values
        ## levels(pklong4[,unique(get(col.var))])
        ## the ones to include in the scale
        ## pklong4[!is.na(get(col.val)),unique(get(col(var)))]
        ## the factor orders of these values
        Nval.fac.type <- as.numeric(pklong4[!is.na(get(col.val)),unique(get(col.var))])
        ## the names of these values
        names.levels.type <- as.character(pklong4[!is.na(get(col.val)),unique(get(col.var))])
        ## the number of factor levels
        n.levels.type <- length(levels(pklong4[,unique(get(col.var))]))
        values.fac.type <- rep(NA_integer_,n.levels.type)
        values.fac.type[Nval.fac.type] <- values[Nval.fac.type]

        list(names.levels.type=names.levels.type,
             values.fac.type=values.fac.type)
    }
    
    scalevals.type <- genScaleLevels("val.line","type",values=seq(1,4,1))
    scalevals.shape <- genScaleLevels("val.point","variable",values=seq(1,4,1))
    scalevals.linetype <- genScaleLevels("val.line","variable",values=seq(1,4,1))

    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    if("mean"%in%pklong4$ID) {
        l.ID <- levels(pklong4$ID)
        pklong4[,ID:=factor(as.character(pklong4$ID),
                            levels=c("mean",l.ID[l.ID!="mean"])
                            )]
        colours <- c("#000000",gg_color_hue(length(unique(pklong$ID))))
    } else {
        colours <- gg_color_hue(length(unique(pklong$ID)))
    }
    scalevals.colour <-
        genScaleLevels(c("val.line","val.point"),"ID",values=colours)
    
    ggplot(pklong4) +
        geom_line(aes_string(col.time,y="val.line",colour="ID",linetype="variable",size="type")) +
        geom_point(aes_string(col.time,"val.point",colour="ID",size="type")) +
        geom_errorbar(aes_string(col.ntim,ymin="val.ci.l",ymax="val.ci.u",linetype="variable")) +
        ## geom_linerange(aes(NOMTIME,ymin=mean.grp.l,ymax=mean.grp.u,linetype=variable),data=function(d)d[variable=="DV"])+
        scale_size_manual(breaks=scalevals.type$names.levels.type,values=scalevals.type$values.fac.type,name=NULL) +
        scale_shape_manual(breaks=scalevals.shape$names.levels.type,values=scalevals.shape$values.fac.type,name=NULL) +
        scale_linetype_manual(breaks=scalevals.linetype$names.levels.type,values=scalevals.linetype$values.fac.type,name=NULL) +
        scale_colour_manual(breaks=scalevals.colour$names.levels.type,values=scalevals.colour$values.fac.type)

    
}

