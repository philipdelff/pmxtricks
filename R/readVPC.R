## This function is not ready yet, so not exported

readVPC <- function(file,ifMissing=NULL,debug=F){
    if(debug) browser()

    if(!file.exists(file)) {
        warning("file does not exist.")
        return(ifMissing)
    }
    
    library(xpose4)
    vpc1 <- xpose4::read.npc.vpc.results(vpc.results=file)

    stratified <- FALSE

    if(!is.null(vpc1$strata)){    
        stratified <- TRUE
        nstrata <- length(vpc1$strata.names)
        if(length(vpc1$result.tables)!=nstrata) {
            stop("number of tables not equal to number of strata. Don't know how to handle this.")
        }
        
        
        data <- lapply(1:nstrata,function(x){
            df <- vpc1$result.tables[[x]]
            df[,"strata"] <- vpc1$strata.names[x]
            names(df) <- paste0("X",names(df))
            df
        })

        
        vpc2 <- data.table::rbindlist(data)
        res.vpc <- vpc2
    } else {
        df <- vpc1$result.tables
        names(df) <- paste0("X",names(df))
        res.vpc <- data.table(df)
    }

    ## res.vpc
    
    ## vpc.l <- melt(res.vpc,id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"))
    ## vpc.l

    vpc.l2 <- data.table::melt(res.vpc,measure.vars=c("X50.real","X50.sim"))

    ## res.vpc
    dd1 <- data.table::melt(res.vpc,
                ## id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
                measure.vars = patterns("^X10","^X50","^X90"),
                value.name = c("X10","X50","X90")
                )

    dd2 <- merge(dd1,
                 data.table(variable=1:2,
                            var=c("real","sim"))
                 )


    
    dd2[var=="real",X95.CI.for.10.from:=NA]
    dd2[var=="real",X95.CI.for.10.to:=NA]
    dd2[var=="real",X95.CI.for.50.from:=NA]
    dd2[var=="real",X95.CI.for.50.to:=NA]
    dd2[var=="real",X95.CI.for.90.from:=NA]
    dd2[var=="real",X95.CI.for.90.to:=NA]

    
    pvpc <- ggplot(dd2,aes(x=Xnobs))+
        geom_line(aes(y=X50,colour=var),size=1)+
        geom_line(aes(y=X90,colour=var),size=1)+
        geom_line(aes(y=X10,colour=var),size=1)+
        ##        geom_line(aes(y=X50.sim),colour=2,size=2)+
        ##        geom_line(aes(y=X10.sim),colour=2,size=2)+
        ##        geom_line(aes(y=X90.sim),colour=2,size=2)+
        geom_ribbon(aes(ymin=X95.CI.for.50.from,ymax=X95.CI.for.50.to,fill=var),alpha=.5)+
        geom_ribbon(aes(ymin=X95.CI.for.10.from,ymax=X95.CI.for.10.to,fill=var),alpha=.5)+
        geom_ribbon(aes(ymin=X95.CI.for.90.from,ymax=X95.CI.for.90.to,fill=var),alpha=.5)

    if(stratified) {
        pvpc <- pvpc + facet_wrap(~Xstrata)
    }
    
    out <- list(
        data=dd2,
        plot=pvpc
    )

    return(out)
    

    ## browser()

    
    
    ## pvpc <- ggplot(res.vpc,aes(x=Xnobs))+
    ##     geom_line(aes(y=X50.real),size=2)+
    ##     geom_line(aes(y=X90.real),size=2)+
    ##     geom_line(aes(y=X10.real),size=2)+
    ##     geom_line(aes(y=X50.sim),colour=2,size=2)+
    ##     geom_line(aes(y=X10.sim),colour=2,size=2)+
    ##     geom_line(aes(y=X90.sim),colour=2,size=2)+
    ##     geom_ribbon(aes(ymin=X95.CI.for.50.from,ymax=X95.CI.for.50.to),alpha=.5,fill=3)+
    ##     geom_ribbon(aes(ymin=X95.CI.for.10.from,ymax=X95.CI.for.10.to),alpha=.5,fill=3)+
    ##     geom_ribbon(aes(ymin=X95.CI.for.90.from,ymax=X95.CI.for.90.to),alpha=.5,fill=3)



}

if(F){
    dd <- melt(res.vpc,
               id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
               ##      measure.vars = patterns( "\\.from$","\\.to$"),
               measure.vars = list("X95.CI.for.10.from", "X95.CI.for.10.to"),
               value.name = c("from","to")
               )
    dd


    
    dd <- melt(res.vpc,
               id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
               measure.vars = data.table:::patterns( "\\.from$","\\.to$","\\.real$","\\.sim$"),
                                        #               value.name = c("from","to")
               )
    dd


    dd <- melt(res.vpc,
               id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
               measure.vars = data.table:::patterns("^X95.CI.for.10", "^X95.CI.for.50", "^X95.CI.for.90"),
               ##               value.name = c("from","to")
               )
    dd

    dd1 <- melt(res.vpc,
                id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
                measure.vars = data.table:::patterns("^X50"),
                value.name = c("X50")
                )
    dd1


    res.vpc
    dd1 <- melt(res.vpc,
                id.vars=c("Xlower","Xupper","Xnobs","Xno.of.obs","Xmean.real","Xmean.sim","Xstrata"),
                measure.vars = data.table:::patterns("^X10","^X50","^X90"),
                value.name = c("X10","X50","X90")
                )
    dd1
    merge(dd1,
          data.table(variable=1:2,
                     var=c("real","sim"))
          )

    
    
    
    

}
