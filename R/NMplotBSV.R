##' Generate distribution plots of between-occasion variability terms
##' from Nonmem
##' @param data A dataset - will be converted to data.frame so
##'     data.table is OK.
##' @param regex.eta A regular expression defining the naming of the
##'     ETA's of interest.
##' @param col.id The name of the id column in data. Default is ID
##'     like Nonmem.
##' @param covs.num Names of columns containing numerical covariates
##'     to plot the random effects against.
##' @param covs.char Names of columns containing categorical
##'     covariates to plot the random effects against.
##' @param fun.file If saving plots, this function can be used to
##'     translate the file names. The inputs given to the function
##'     argument are "iov_pairs.png" and "iov_covs_n.png".
##' @param save Save the generated plots?
##' @param stamp If saving the plots, a stamp to add. See ggstamp.
##' @param return.data If TRUE, the identified ETA's together with
##'     subject id and covariates will be returned in both wide and
##'     long format. If FALSE, you just get the plots.
##' @param debug Start by running browser()?
##' @import ggplot2
##' @import data.table
##' @import stats
##' @importFrom GGally ggpairs
##' @family Plotting
##' @export

NMplotBSV <- function(data,regex.eta="^ETABSV",col.id="ID",covs.num,covs.char,fun.file=identity,save=FALSE,stamp=NULL,return.data=FALSE,debug=F){

    if(debug) {browser()}
    
    pkpars <- as.data.table(data)

    if(missing(covs.num)) covs.num <- NULL
    if(missing(covs.char)) covs.char <- NULL

#### Section start: dummy variables, only not to get NOTE's in pacakge checks ####

    value  <- NULL
    ..density..  <- NULL
    predicted <- NULL
    val.cov <- NULL

### Section end: dummy variables, only not to get NOTE's in pacakge checks



    
    all.output <- list()
    
    names.etas <-
        names(pkpars)[
            grep(regex.eta,names(pkpars))
        ]

    ## if specified to be a covariate, drop the eta
    names.etas <- setdiff(names.etas,c(covs.num,covs.char))
    
    ## only the ones that vary
    ## names.etas.var <- names(which(
    ##     sapply(pkpars[,names.etas,with=F],function(x)length(unique(x)))  > 1
    ## ))
    names.etas.var <- colnames(findVars(pkpars[,names.etas,with=F]))

    etas <- NULL
    etas.l <- NULL
    
    if(length(names.etas.var)){
        ##        etas <- pkpars[,c("ID", names.etas.var,covs.num,covs.char)]
        etas <- unique(pkpars[,c("ID", names.etas.var,covs.num,covs.char),with=F])
        
### etas against each other. Notice, all etas, even those = 0.

        points.and.smooth <- function(data, mapping, method="lm", ...){
            p <- ggplot(data = data, mapping = mapping) + 
                geom_point() + 
                geom_smooth(method=method, formula=y~x, ...)
            p
        }

        
        iiv.pairs <- ggpairs(etas,columns=names.etas.var,lower=list(continuous=points.and.smooth))
        
        ggwrite(iiv.pairs,file=fun.file("iiv_pairs.png"),save=save,stamp=stamp)
        all.output[["iiv.pairs"]]  <- iiv.pairs
        
        ## etas.l <- gather(etas,param,value,-1)
        etas.l <- melt(etas,id.vars=c(col.id,covs.num,covs.char),measure.vars=names.etas.var,value.name="value",variable.name="param")
        ##
        ## compare.names(etas,pkpars)
        ##   etas.l <- mergeCheck(etas.l,pkpars,by=c(col.id,covs.num,covs.char),allow.cartesian=TRUE)
        
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

        DT.dat <- as.data.table(dat)
        normaldens <-
            DT.dat[,list(
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
        if(!is.null(covs.num)){
            if(!all(covs.num%in%names(pkpars))){
                covs.num.drop <- setdiff(covs.num,names(pkpars))
                warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
                covs.num <- setdiff(covs.num,covs.num.drop)
            }
            
            ## use only covariates that vary
            ## covs.num <- names(which(
            ##     sapply(pkpars[,covs.num,drop=F],function(x)length(unique(x)))  > 1
            ## ))
            covs.num <- colnames(findVars(pkpars[,covs.num,with=F]))
            
            etas.l2.n <- etas.l[,c(col.id,"param","value",covs.num),with=FALSE]

            if(
                length(setdiff(colnames(etas.l2.n),c("ID","param","value")))>0
            ){
                etas.covs.n <- melt.data.table(etas.l2.n,variable.name="cov",value.name="val.cov",measure.vars=names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])

                p.iiv.covsn <- ggplot(subset(etas.covs.n,!grepl(regex.eta,cov)),aes(val.cov,value))+
                    geom_point()+
                    geom_smooth(method="lm", formula=y~x)+
                    facet_grid(param~cov,scales="free")
                ggwrite(p.iiv.covsn,file=fun.file("iiv_covs_n.png"),save=save,stamp=stamp)
                all.output[["iiv.covsn"]] <- p.iiv.covsn
            }
        }
        if(!is.null(covs.char)){
            
            etas.l2.c <- etas.l[,c(col.id,"param","value",covs.char),with=F]

            DT <- data.table(etas.l2.c)

            DT2 <- melt(DT,measure.vars=covs.char,id.vars=c("ID","param","value"),value.name="val.cov",value.factor=T)
            p.iiv.covsc.dt <- ggplot(DT2,aes(val.cov,value))+geom_boxplot()+facet_wrap(~param)
            ggwrite(p.iiv.covsc.dt,file=fun.file("iiv_covs_c.png"),save=save,stamp=stamp)
            all.output[["iiv.covsc"]] <- p.iiv.covsc.dt
        }
    } else {
        message("No IIV random effects found in parameter table.")
    }
    if(return.data){
        all.output[["etas"]] <- etas
        all.output[["etas.l"]] <- etas.l
    }
    all.output
}
