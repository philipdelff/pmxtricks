### this file includes both modelOverview and modelOverviewAll

##' goodness of fit and more
##'
##' This function is far from done. The idea is it makes a whole bunch
##' of plots and tables as an overview of the performance of a nonmem
##' model.

#### todo
## LLOQ is assumed to be a column

## It may only work if IOV is present

## better detection of tables and table types

## done save individual profiles in pdf as onefile as well

## done in ggIndProf, it should be possible to control no of subjects per page.

## save all plots and tables in a pdf
#### todo end

modelOverview <- function(path.run,str.output="NMfolders",folder.out,col.id,covs.num,covs.char,var.occ="OCC",var.occ.char=var.occ,covs.num.iov,covs.char.iov,write.output,GRP="GRP",grp=GRP,stamp=NULL,logy=T,LLOQ,
                          args.ggIndProf=NULL,grp.trick=NULL,
                          regex.iiv="^ETAIIV",
                          debug=F){

###{ checks of arguments
    if(is.logical(debug)&&debug) debug <- 0
    debug.pos <- 0
    if(!is.logical(debug)&&debug==debug.pos) {browser()}
    if(!dir.exists(path.run)) stop("The supplied run could not be found.")

    if(missing(covs.num.iov)) covs.num.iov <- NULL
    if(missing(covs.char.iov)) covs.char.iov <- NULL
    if(!is.null(grp.trick)){
        ##     grp <- grp.trick
        GRP <- NULL
    }
###}

###{ where to store the output under dir.plots, dir.tables
#### Option 1 -
### output folders shared by all models - one in dir.plots, one in dir.tables
### files names by model name
    name.run <- sub(".*/(.+)$","\\1",path.run)
    if(str.output=="flat"){
        if(missing(folder.out)) stop("when str.output=flat, folder.out is needed.")
        ##        folder.out <- paste("PK",drug,sep="_")
        file.runplot <- function(name.run,string) file.plots(folder.out,paste(name.run,string,sep="_"))
        file.runtab <- function(name.run,string) file.tables(folder.out,paste(name.run,string,sep="_"))
### make sure needed folders exist
        if(!dir.exists(file.plots(folder.out))) dir.create(file.plots(folder.out))
        if(!dir.exists(file.tables(folder.out))) dir.create(file.tables(folder.out))

    }

#### Option 2 - output saved within the nonmem run folder.
    if(str.output=="NMfolders"){
        ##        rundir <- list.files(folder.runs,pattern=mod,full.names=T)
        ##        folder.out <- file.path(rundir,"model_summary")
        folder.out <- file.path(path.run,"model_summary")
        file.runplot <- function(name.run,string) file.path(folder.out,paste("fig",string,sep="_"))
        file.runtab <- function(name.run,string) file.path(folder.out,paste("tab",string,sep="_"))
### make sure needed folders exist
        if(!dir.exists(folder.out)) dir.create(folder.out)
    }

    all.output <- list()
###}

    
###{ read data
    message("Reading data tables from Nonmem run")
    debug.pos <- 1
    if(debug==debug.pos) {browser()}
    
    ## file.input <- file.derived("nm_pkdata_v1.rds")
    ## pkres <- NMreconstruct(output=file.path(path.run,"TABLE_OUTPUT.txt"),
    ##                        input=file.input)


    ## ## source("e:/Users/ppda/R_functions_ppda/NMreconstruct.R")
    ## pkpars <- NMreconstruct(output=file.path(path.run,"TABLE_PARAM_ALLROWS.txt"),
    ##                              input=file.input,
    ##                              type="pars",
    ##                              col.id=c(col.id,GRP),debug=F)

    ## pkiov <- NULL
    ## file.iov <- file.path(path.run,"TABLE_IOV.txt")
    ## if(file.exists(file.iov))
    ## pkiov <- NMreconstruct(output=file.iov,
    ##                        input=file.input,type="iov",col.id=c(col.id,GRP),
    ##                        col.occ=var.occ)
    data <- try(NMscan(path.run,strings.tables=list(output="OUT",pars="PARAM",iov="IOV"),col.grp=GRP,col.occ=var.occ))
    if("try-error"%in%class(data)) {
        message("No nonmem output tables found. Nonmem run succesfully finished?")
        return(invisible())
    }
    ## browser()
    
    if(!is.null(grp.trick)) {
        data <- lapply(data,function(x){
        x$data$GRP <- 1
        x$data$grp <- grp.trick
        x}
        )
        grp <- "grp"
        GRP <- "GRP"
    }
        
    
    message("Reading data tables done")
###}

###{ GOF plots panel
    message("QCP panel of GOF plots")
    debug.pos <- 2
    if(debug==debug.pos) {browser()}

    pkres <- data$output$data

    ## if(is.null(grp)) grp <- "grptmp"
    ## pkres[,grp] <- 1
    ## pkres$GRP <- pkres[,grp]

    logxy <- ""
    if(logy) logxy <- "xy"
    fn <- NULL
    if(write.output) {
        fn <- file.runplot(name.run,"NMplot_GOF.pdf")
    }
    pkres.gof <- subset(pkres,EVID==0)
    if(is.null(grp)) pkres.gof$GRP <- NULL
    NMplot.GOF(pkres.gof,export.file=fn,stamp=stamp,log=logxy,landscape=T)
    if(write.output) {
        dev.off()
        fn <- file.runplot(name.run,"NMplot_GOF.png")
        NMplot.GOF(pkres.gof,export.file=fn,stamp=stamp,log=logxy,landscape=T)
        dev.off()
    }

###}



###{ Parameter table
    message("Creating parameter table")
    debug.pos <- 3
    if(debug==debug.pos) {browser()}

    
    ## getandsource("e:/Toolbox/CodeDatabase/QCPfunc_Dev/QCPfunc/R/TableOfEstimates.R",dir.local="functions_QCPfunc",source.directly=T)
    ## getandsource("e:/Toolbox/CodeDatabase/QCPfunc_Dev/QCPfunc/R/TableOfEstimates.R",dir.local="functions_QCPfunc",overwrite=T)


    tab1 <- try(
        TableOfEstimates(run.dir=path.run,theta.strip.tv=T)
    )
    if("try-error"%in%class(tab1)){
        warning(paste("Parameter table could not be generated. The error was:\n",as.character(tab1)))
    } else {
        if(write.output){
            write.csv(tab1,file.runtab(name.run,"param_tab.csv"))

            has.grid <- require(grid)
            if(has.grid){
                ##    tab1.grid <- grid.table(tab1)
                ##                tab1.strip <- tab1
                ##                rownames(tab1.strip) <- NULL
                tab1.grid <- tableGrob(tab1,rows=NULL)
                gsave(tab1.grid,file=file.runtab(name.run,"param_tab.png"),stamp=stamp,save=T,canvas="wide")
                all.output <- c(all.output,tab1.grid)
            } else {
                message("Package \"grid\" not available. Not saving parameter table as png.")
            }
        } else {
            print(tab1)
        }
    }
    message("Creating parameter table done\n")
###}

    
    ##dim(pkpars)


### find n subjects on each arm, subj on each profid x arm. This belongs before Nonmem runs.
    ## group_by(pkres,DRUG) %>% summarise(n_distinct(ID))
    ## group_by(pkres,DRUG,PROFID) %>% summarise(Nprofiles=n_distinct(ID))
    ## group_by(pkres,DRUG,VISIT) %>% summarise(N.IDs=n_distinct(ID))


###{ model compared to data. 
    message("plots of data and predictions against time...")
    debug.pos <- 4
    if(debug==debug.pos) {browser()}

    ## dim(pkres)
    ##browser()
    pkres <- data$output$data
    if(!missing(LLOQ)) pkres$LLOQ <- LLOQ
    
    pkres.l <- gather(pkres,param,value,PRED,IPRE,DV)
    pkres.l <- merge.and.check(pkres.l,data.frame(param=c("PRED","IPRE","DV"),type=c("pred","pred","obs")))
    pkres.l.obs <- subset(pkres.l,EVID%in%c(0,2))
    p1 <- ggplot(subset(pkres.l.obs))+
### will be the same since FLAG!=0 discarded
        ## stat_summary(aes(NTIM,DVPLOT),fun.y=geoMean,colour=2,geom="point")
        geom_line(aes_string("TIME","value",group=paste0("interaction(", paste0(c(col.id,var.occ.char), collapse = ", "), ")")),data=subset(pkres.l.obs,param=="DV"),alpha=.5)+
        ##        geom_line(aes(TIME,value,group=interaction(ID,profile_id)),data=subset(pkres.l.obs,param=="DV"),alpha=.5)+
        geom_point(aes(TIME,value),data=subset(pkres.l.obs,param=="DV"),alpha=.5)+
        geom_hline(aes(yintercept=LLOQ),linetype=2,data=subset(pkres.l.obs,EVID==0))
    if(!is.null(grp)){
        p1 <- p1+facet_grid(as.formula(paste0(".~",grp)))
    }

    if(logy){
        p1 <- p1+
            stat_summary(aes(NTIM,value,colour=param),fun.y=geoMean,geom="line",size=2,data=subset(pkres.l.obs,NTIM>=0))+
            scale_y_log10(limits=c(min(pkres.l.obs$LLOQ[pkres.l.obs$LLOQ>0])/1.1,NA))
        ##            scale_color_discrete(NNcolour(2:4))
    } else {
        p1 <- p1+
            stat_summary(aes(NTIM,value,colour=param),fun.y=mean,geom="line",size=2,data=subset(pkres.l.obs,NTIM>=0))
    }
    p2 <- p1+scale_x_log10()
    ## missing: confint obs

    ## t1 <- grid.arrange(p1,p2,nrow=1)
    ## t2 <- grid.arrange(p3,p4,nrow=1)

    ## getandsource("e:/Toolbox/CodeDatabase/QCPfunc_Dev/QCPfunc/R/gsave.R",dir.local="functions_QCPfunc",overwrite=T)
    ## getandsource("e:/Toolbox/CodeDatabase/QCPfunc_Dev/QCPfunc/R/ggstamp.R",dir.local="functions_QCPfunc",overwrite=T)

    gsave(p1,file=file.runplot(name.run,"obs_pred_ipre.png"),stamp=stamp,save=write.output,debug=F)
    gsave(p2,file=file.runplot(name.run,"obs_pred_ipre_xlog.png"),stamp=stamp,save=write.output)

#### facet by profile_id
    if(is.null(grp)){
                p1p <- p1+facet_grid(as.formula(paste(".",var.occ.char,sep="~")))
                p2p <- p2+facet_grid(as.formula(paste(".",var.occ.char,sep="~")))
    } else {
        p1p <- p1+facet_grid(as.formula(paste(grp,var.occ.char,sep="~")))
        p2p <- p2+facet_grid(as.formula(paste(grp,var.occ.char,sep="~")))
    }
    ## p3p <- p3+facet_grid(as.formula(paste(grp,var.occ.char,sep="~")))
    ## p4p <- p4+facet_grid(as.formula(paste(grp,var.occ.char,sep="~")))

    ## t1p <- grid.arrange(p1p,p2p,ncol=1)
    ## t2p <- grid.arrange(p3p,p4p,ncol=1)

    gsave(p1p,file=file.runplot(name.run,"obs_pred_ipre_prof.png"),stamp=stamp,save=write.output,debug=F)
    gsave(p2p,file=file.runplot(name.run,"obs_pred_ipre_xlog_prof.png"),stamp=stamp,save=write.output)

    ## one plot per id
    ##    pkt <- mutate(pkres,GRP=drug)
    gg1 <- do.call(function(...) ggIndProf(subset(pkres,EVID%in%c(0,2)),par.prof=var.occ.char,grp=grp,logy=logy,debug=F,...),args.ggIndProf)
    ## gg1[[1]]

    gsave(gg1,file=file.runplot(name.run,"individual_profiles.png"),stamp=stamp,save=write.output)
    if(write.output){
        gsave(gg1,file=file.runplot(name.run,"individual_profiles.pdf"),onefile=T,stamp=stamp,save=write.output)
    }
    all.output <- c(all.output,p1,p2,p1p,p2p)
    message("plots of data and predictions against time done.")
###}


###{ plot IIV random effects
    message("IIV random effects plots...")
    debug.pos <- 5
    if(debug==debug.pos) {browser()}

    pkpars <- data$pars$data
    names.etas <-
        names(pkpars)[
            grep(regex.iiv,names(pkpars))
        ]

    ## if specified to be a covariate, drop the eta
    names.etas <- setdiff(names.etas,c(covs.num,covs.char,covs.num.iov))
    
    ## only the ones that vary
    names.etas.var <- names(which(
        sapply(pkpars[,names.etas],function(x)length(unique(x)))  > 1
    ))

    if(length(names.etas.var)){
        etas <- pkpars[,c("ID", names.etas.var)]
        
### etas against each other. Notice, all etas, even those = 0.

        points.and.smooth <- function(data, mapping, method="lm", ...){
            p <- ggplot(data = data, mapping = mapping) + 
                geom_point() + 
                geom_smooth(method=method, ...)
            p
        }

        
        iiv.pairs <- ggpairs(etas,columns=names.etas.var,lower=list(continuous=points.and.smooth))
        
        gsave(iiv.pairs,file=file.runplot(name.run,"iiv_pairs.png"),stamp=stamp,save=write.output)
        all.output <- c(all.output,iiv.pairs)
        
        etas.l <- gather(etas,param,value,-1)
        ##
        ## compare.names(etas,pkpars)
        etas.l <- merge.and.check(etas.l,pkpars)
        
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
        normaldens <- ddply(dat, "param", function(df) {
            data.frame( 
                predicted = grid,
                density = dnorm(grid, mean(df[,"value"]), sd(df[,"value"]))
            )
        })
        
        gh2 <- ggplot(data = dat,aes(x = value)) + 
            geom_histogram(aes(y = ..density..)) + 
            geom_line(data = normaldens, aes(x = predicted, y = density), colour = "red",size=1)+
            facet_wrap(~param,scales="free")

        gsave(gh2,file=file.runplot(name.run,"hists_etas_actual_wgaussian.png"),save=write.output,stamp=stamp)
        all.output <- c(all.output,gh2)

        fn <- NULL
        if(write.output) {
            fn <- file.runplot(name.run,"qq_etas_NMplot.png")
        }
        NMplot.qq(pkpars,par.cols=names.etas.var,export.file=fn)
        ## if(write.output) dev.off()

        fn <- NULL
        if(write.output) {
            fn <- file.runplot(name.run,"hists_etas_NMplot.png")
        }
        NMplot.hist(pkpars,par.cols=names.etas.var,export.file=fn)
        ## if(write.output) dev.off()
        
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
        
        etas.l2.n <- merge.and.check(etas.l,unique(pkpars[c(col.id,covs.num)]))
        etas.l2.n <- etas.l2.n[,c(col.id,"param","value",covs.num)]
        etas.covs.n <- gather_(etas.l2.n,"cov","val.cov",names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])
        if(any(grepl("^ETA",covs.num))){
            p.iiv.covsn.eta <- ggplot(subset(etas.covs.n,grepl("^ETA",cov)),aes(val.cov,value))+geom_point()+
                geom_smooth(method="lm")+
                facet_grid(param~cov,scales="free")
            gsave(p.iiv.covsn.eta,file=file.runplot(name.run,"iiv_covs_etas.png"),save=write.output,stamp=stamp)
        }
        p.iiv.covsn <- ggplot(subset(etas.covs.n,!grepl("^ETA",cov)),aes(val.cov,value))+geom_point()+
            geom_smooth(method="lm")+
            facet_grid(param~cov,scales="free")
        gsave(p.iiv.covsn,file=file.runplot(name.run,"iiv_covs_n.png"),save=write.output,stamp=stamp)
        all.output <- c(all.output,p.iiv.covsn)

        ##        browser()
        if(!is.null(covs.char)){
            etas.l2.c <- merge.and.check(etas.l,unique(pkpars[c(col.id,covs.char,drop=F)]))
            etas.l2.c <- etas.l2.c[,c(col.id,"param","value",covs.char)]
            etas.covs.c <- gather_(etas.l2.c,"cov","val.cov",names(etas.l2.c)[!names(etas.l2.c)%in%c("ID","param","value")])
            
            ## p.iiv.covsc <- ggplot(etas.covs.c,aes(colour=val.cov,value))+geom_density()+facet_grid(param~cov,scales="free")
            p.iiv.covsc <- by(etas.covs.c,etas.covs.c$cov,function(data)ggplot(data,aes(colour=val.cov,value))+geom_density()+facet_grid(param~cov,scales="free"))
        
            gsave(p.iiv.covsc,file=file.runplot(name.run,"iiv_covs_c.png"),save=write.output,stamp=stamp)
            all.output <- c(all.output,p.iiv.covsc)
        }
    } else {
        message("No IIV random effects found in parameter table.")
    }
    message("IIV random effects plots done.")
###}

###{ IOV random effects
    message("IOV random effects plots...")
    debug.pos <- 6
    if(debug==debug.pos) {browser()}
    
    pkiov <- data$iov$data
    if(!is.null(pkiov)){
        ##        browser()
        names.etas.iov <- names(pkiov)[grep("^ETAIOV",names(pkiov))]
        ## if specified to be a covariate, drop the eta

        names.etas.iov <- setdiff(names.etas.iov,c(covs.num.iov,covs.char.iov))
        if(length(names.etas.iov)){
            ## only the ones that vary
            names.etas.iov.var <- names(which( sapply(pkiov[,names.etas.iov],function(x)length(unique(x)))  > 1  ))
            ##            browser()
            if (length(names.etas.iov.var)){
                if(length(names.etas.iov.var)==1){
                    pairs.iov <- ggplot(pkiov,aes_string(names.etas.iov.var,fill=var.occ.char))+geom_density(alpha=.5)
                } else  {
                    pairs.iov <- ggpairs(pkiov,columns=names.etas.iov.var,mapping=aes_(colour=as.name(var.occ.char),alpha=.5))
                }
                gsave(pairs.iov,file=file.runplot(name.run,"iov_pairs.png"))
                all.output <- c(all.output,pairs.iov)
            }
### IOV random effects vs. possible occasion covariates
            etas.iov <- pkiov[,c(col.id,var.occ,names.etas.iov.var)]
            if(length(names.etas.iov.var)&&!is.null(covs.num.iov)){
                if(!all(covs.num.iov%in%names(pkiov))){
                    covs.num.drop <- setdiff(covs.num.iov,names(pkiov))
                    warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
                    covs.num.iov <- setdiff(covs.num.iov,covs.num.drop)
                }

                ##                browser()
### only plot against covs that vary within subjects
                covs.num.iov <- names(which(
                    sapply(pkiov[,covs.num.iov],function(x)length(unique(x)))  > 1))
                
                etas.iov.l <- gather(etas.iov,param,value,-c(1:2))
                etas.l2.n <- merge.and.check(etas.iov.l,unique(pkiov[c(col.id,var.occ,covs.num.iov)]))
                etas.l2.n <- etas.l2.n[,c(col.id,var.occ,"param","value",covs.num.iov)]
                etas.covs.n <- gather_(etas.l2.n,"cov","val.cov",names(etas.l2.n)[!names(etas.l2.n)%in%c(col.id,var.occ,"param","value")])
                
                p.iov.covsn <- ggplot(etas.covs.n,aes(val.cov,value))+geom_point()+facet_grid(param~cov,scales="free")+geom_smooth(method="lm")    
                
                gsave(p.iov.covsn,file=file.runplot(name.run,"iov_covs_n.png"),save=write.output,stamp=stamp)
            }
            if(!is.null(covs.char.iov)){
                warning("covs.char.iov supplied. This is not implemented.")
            }
            
        } else {
            message("No inter-occasion variability found in iov table")
        }
    } else {
        message("No inter-occasion variability table found")
    }

    message("IOV random effects plots done.")
###}

###{ Save as much output as possible in one file
    debug.pos <- 6
    if(debug==debug.pos) {browser()}

    ## if(write.output){
    ##     gsave(all.output,file=file.runplot(name.run,"most_outputs.pdf"),onefile=T,stamp=NULL)
    ## }

###}
}

###{ model.diag.all - run model.diagnostics on many runs. But only if it hasn't been done already.
#' @param dirs Regular expression. Only directories whose name match
#'     this will be taken into account.
modelOverviewAll <- function(path,debug=F,pattern.dirs="^[0-9].*",force=FALSE,...){
    if(debug) browser()

    modeldirs <- list.files(path,pattern=pattern.dirs)
    ## todo Check that dirs found

### todo: print the ones that are omitted due to missing table files.
### find existing all outputfile
    ## allouts <- unlist(lapply(outputfile,function(filename)
    ##     list.files(file.path(path,modeldirs),pattern=paste(filename,"$",sep=""),recursive=T,full.names=T)
    ##     ))

    allouts <- list.files(file.path(path,modeldirs),pattern=paste("^output.txt$"),recursive=T,full.names=T)
    
    ##    filenames <- basename(allouts)

    ## reduce by removing the ones that contain a non-empty dir called
    ## model_diagnostics
    
    up2date <- sapply(dirname(allouts),function(dir){
        if(force) {
            return(FALSE)
        } else {
            dir.diag <- file.path(dir,"model_summary")
            return(dir.exists(dir.diag)&&length(list.files(dir.diag)))
        }
    })
    allouts <- allouts[!up2date]
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    
#### exclude models that are still running
    running <- sapply(allouts,function(outfile)length(list.files(dirname(outfile),pattern="^OUTPUT$"))>0)
    allouts <- allouts[!running]
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    
#### some problems lead to huge output.txt files. We dont want to search those.
    file.ok <- sapply(allouts,function(outfile){
        file.size(outfile)<2e5
    })
    if(any(!file.ok)) warning ("an output.txt was too large. Check this yourself.")
    allouts <- allouts[file.ok]
    
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    ##    filenames <- basename(allouts)


    
    ## these are relative to path
    allpaths <- dirname(allouts)

    silent <- lapply(1:length(allpaths),function(N){
        print(allpaths[N])
        modelOverview(path=allpaths[N],...)
    }
    )
    return(invisible())
}

