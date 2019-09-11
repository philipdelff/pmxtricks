
##' @export

NMplotBSV <- function(data,regex.eta="^ETABSV",col.id="ID",covs.num,covs.char,fun.file=identity,save=FALSE,stamp=NULL,debug=F){###{ plot IIV random effects
    if(debug) {browser()}

    library(data.table)
    
    pkpars <- as.data.frame(data)

    if(missing(covs.num)) covs.num <- NULL
    if(missing(covs.char)) covs.char <- NULL

    all.output <- list()
    
    names.etas <-
        names(pkpars)[
            grep(regex.eta,names(pkpars))
        ]

    ## if specified to be a covariate, drop the eta
    names.etas <- setdiff(names.etas,c(covs.num,covs.char))
    
    ## only the ones that vary
    names.etas.var <- names(which(
        sapply(pkpars[,names.etas],function(x)length(unique(x)))  > 1
    ))

    etas <- NULL
    etas.l <- NULL
    
    if(length(names.etas.var)){
        etas <- pkpars[,c("ID", names.etas.var,covs.num,covs.char)]
        
### etas against each other. Notice, all etas, even those = 0.

        points.and.smooth <- function(data, mapping, method="lm", ...){
            p <- ggplot(data = data, mapping = mapping) + 
                geom_point() + 
                geom_smooth(method=method, ...)
            p
        }

        
        iiv.pairs <- ggpairs(etas,columns=names.etas.var,lower=list(continuous=points.and.smooth))
        
        ggwrite(iiv.pairs,file=fun.file("iiv_pairs.png"),save=save,stamp=stamp)
        all.output[["iiv.pairs"]]  <- iiv.pairs
        
        ## etas.l <- gather(etas,param,value,-1)
        etas.l <- melt(etas,id.vars=c(col.id,covs.num,covs.char),measure.vars=names.etas.var,value.name="value",variable.name="param")
        ##
        ## compare.names(etas,pkpars)
        etas.l <- mergeCheck(etas.l,pkpars)
        
        ## g1 <- ggplot(etas.l,aes(value))+
        ##     geom_histogram()+
        ##     facet_wrap(~param,scales="free")
        ## save this on
        ## gsave(g1,file=file.runplot(name.run,"hists_etas.png"),save=write.output,stamp=stamp)

        etas.l.actual <- subset(etas.l,value!=0)
        ## g2 <- g1%+%etas.l.actual +  facet_wrap(~param,scales="free")
        ## gsave(g2,file=file.runplot(name.run,"hists_etas_actual.png"),save=write.output,stamp=stamp)

#### this is the histograms of non-zeros and with gaussian approximations
        dat <- etas.l.actual

        grid <- with(dat, seq(min(value), max(value), length = 100))
        ## normaldens <- ddply(dat, "param", function(df) {
        ##     data.frame( 
        ##         predicted = grid,
        ##         density = dnorm(grid, mean(df[,"value"]), sd(df[,"value"]))
        ##     )
        ## })

        DT.dat <- as.data.table(dat)
        normaldens <-
            DT.dat[,.(
            predicted=grid,
            density=dnorm(grid, mean(value), sd(value))
        ),
        by="param"]
        
        gh2 <- ggplot(data = dat,aes(x = value)) + 
            geom_histogram(aes(y = ..density..)) + 
            geom_line(data = normaldens, aes(x = predicted, y = density), colour = "red",size=1)+
            facet_wrap(~param,scales="free")

        ## gsave(gh2,file=file.runplot(name.run,"hists_etas_actual_wgaussian.png"),save=write.output,stamp=stamp)
        ggwrite(gh2,file=fun.file("hists_etas_actual_wgaussian.png"),save=save,stamp=stamp)
        all.output[["hists.etas"]]  <- gh2

        plot.qq <- ggplot(dat,aes(sample=value))+
            geom_qq_line(colour=2,size=1.5)+
            geom_qq()+
            facet_wrap(~param)+
            labs(x="Theoretical",y="Observed")
        ggwrite(plot.qq,file=fun.file("qq_etas.png"),save=save,stamp=stamp)
        all.output[["qq.bsv"]]  <- plot.qq
        
        ## IIV random effects vs covariates
        if(!all(covs.num%in%names(pkpars))){
            covs.num.drop <- setdiff(covs.num,names(pkpars))
            warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
            covs.num <- setdiff(covs.num,covs.num.drop)
        }

        ## use only covariates that vary
        covs.num <- names(which(
            sapply(pkpars[,covs.num,drop=F],function(x)length(unique(x)))  > 1
        ))
        
        etas.l2.n <- mergeCheck(etas.l,unique(pkpars[c(col.id,covs.num)]))
        etas.l2.n <- etas.l2.n[,c(col.id,"param","value",covs.num)]

        if(length(names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])){
        etas.covs.n <- gather_(etas.l2.n,"cov","val.cov",names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])
### I dont understand why this is needed. This should just be a numeric covariate like any other, I guess?
        ## if(any(grepl("^ETA",covs.num))){
        ##     p.iiv.covsn.eta <- ggplot(subset(etas.covs.n,grepl("^ETA",cov)),aes(val.cov,value))+geom_point()+
        ##         geom_smooth(method="lm")+
        ##         facet_grid(param~cov,scales="free")
        ##     gsave(p.iiv.covsn.eta,file=file.runplot(name.run,"iiv_covs_etas.png"),save=write.output,stamp=stamp)
        ## }
        p.iiv.covsn <- ggplot(subset(etas.covs.n,!grepl(regex.eta,cov)),aes(val.cov,value))+
            geom_point()+
            geom_smooth(method="lm")+
            facet_grid(param~cov,scales="free")
        ggwrite(p.iiv.covsn,file=fun.file("iiv_covs_n.png"),save=save,stamp=stamp)
        all.output[["iiv.covsn"]] <- p.iiv.covsn
        }
        ##        browser()
        if(!is.null(covs.char)){
            
            etas.l2.c <-
                mergeCheck(etas.l,unique(pkpars[,c(col.id,covs.char),drop=F]))
            etas.l2.c <- etas.l2.c[,c(col.id,"param","value",covs.char)]

#### gather_ does not respect factor levels. Using data.table for this melt/gather.
            ## etas.covs.c <- gather_(etas.l2.c,"cov","val.cov",names(etas.l2.c)[!names(etas.l2.c)%in%c("ID","param","value")])
            
            ## p.iiv.covsc <- ggplot(etas.covs.c,aes(colour=val.cov,value))+geom_density()+facet_grid(param~cov,scales="free")
            ## p.iiv.covsc <- by(etas.covs.c,etas.covs.c$cov,function(data)ggplot(data,aes(colour=val.cov,value))+geom_density()+facet_grid(param~cov,scales="free"))
##            p.iiv.covsc <- ggplot(etas.covs.c,aes(val.cov,value))+geom_boxplot()+facet_wrap(~param)
            ## all.output[["iiv.covsc"]] <- p.iiv.covsc
  
            DT <- data.table(etas.l2.c)
            DT[,dose]   

            DT2 <- melt(DT,measure.vars="dose",id.vars=c("ID","param","value"),value.name="val.cov",value.factor=T)
            p.iiv.covsc.dt <- ggplot(DT2,aes(val.cov,value))+geom_boxplot()+facet_wrap(~param)
            ggwrite(p.iiv.covsc.dt,file=fun.file("iiv_covs_c.png"),save=save,stamp=stamp)
            all.output[["iiv.covsc"]] <- p.iiv.covsc.dt
        }
    } else {
        message("No IIV random effects found in parameter table.")
    }
    all.output[["etas"]] <- etas
    all.output[["etas.l"]] <- etas.l
    all.output
}
