##' @export

###{ IOV random effects
## message("IOV random effects plots...")



NMplotBOV <- function(data,regex.eta="^ETABOV",col.id="ID",covs.num.iov,covs.char.iov,var.occ="OCC",var.occ.char=var.occ,fun.file=identity,save=F,stamp=NULL,debug=F){

    if(debug) browser()
    ##    pkiov <- data$iov$data
    pkiov <- as.data.frame(data)
    all.output <- list()
    
    if(missing(covs.num.iov)) covs.num.iov <- NULL
    if(missing(covs.char.iov)) covs.char.iov <- NULL
    ##    names.etas.iov.var <- names(which( sapply(pkiov[,names.etas.iov],function(x)length(unique(x)))  > 1  ))
    
    
    ## if(is.null(pkiov)){
    ##     ##        browser()
    ##     message("No inter-occasion variability table found")
    ##     return(invisible(NULL))
    ## }
    
    names.etas.iov <- names(pkiov)[grep(regex.eta,names(pkiov))]
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
            ggwrite(pairs.iov,file=fun.file("iov_pairs.png"),save=save,stamp=stamp)
            all.output[["pairs.iov"]] <- pairs.iov
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
                sapply(pkiov[,covs.num.iov,drop=F],function(x)length(unique(x)))  > 1
            ))
            
            etas.iov.l <- gather(etas.iov,param,value,-c(1:2))
            etas.l2.n <- mergeCheck(etas.iov.l,unique(pkiov[c(col.id,var.occ,covs.num.iov)]))
            etas.l2.n <- etas.l2.n[,c(col.id,var.occ,"param","value",covs.num.iov)]
            etas.covs.n <- gather_(etas.l2.n,"cov","val.cov",names(etas.l2.n)[!names(etas.l2.n)%in%c(col.id,var.occ,"param","value")])
            
            p.iov.covsn <- ggplot(etas.covs.n,aes(val.cov,value))+geom_point()+facet_grid(param~cov,scales="free")+geom_smooth(method="lm")
            
            ggwrite(p.iov.covsn,file=fun.file("iov_covs_n.png"),save=save,stamp=stamp)
            all.output[["p.iov.covsn"]] <- p.iov.covsn
        }
        if(!is.null(covs.char.iov)){
            warning("covs.char.iov supplied. This is not implemented.")
        }
        
    } else {
        message("No inter-occasion variability found in iov table")
    }
    
    all.output
}
