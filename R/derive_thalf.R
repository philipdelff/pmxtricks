### most of these functions are t1/2 functions. But functions to calculate more derived parameters follow.


##' Thalf for 1 absorption depot, 2 compartments
##'
##' @description Can take multiple parameter sets and return a data.frame of halftimes.
##' @param pars A parameter table. Thalf will be calculated for all rows.
##' @param cl The name of the column with clearance values. Can represent CL/F if volume variables do the same.
##' @param ka The name of the column representing absorption rate.
##' @param v2 The name of the column with central volume (see cl as well).
##' @param q The name of the column with intercompartmental clearance.
##' @param v3 The name of the column with peripharel volume (see cl as well).
##' @param transform Add results to existing dataset?
##' 
##' @param debug Start by calling browser()
## For thalf, I think this is the one to use. Would be good to generalise this to use for simpler systems.
##' @family Calc

thalf_1a2c <- function(pars,cl="CL",ka="KA1",v2="V2",q="Q",v3="V3",transform=F,debug=F){
    if(debug)browser()

    
    varnames <- c(cl,ka,v2,q,v3)
    varsInPars <- varnames%in%names(pars)
    if(any(!varsInPars)){stop(paste("These variables are not in pars:",paste(varnames[!varsInPars],collaps=", ")))}
    
    ## pars.u <- unique(pars[,c(cl,ka,v2,q,v3)])

    thalves <- apply(pars,1,function(pars.u){
        ## Asys <- matrix(unlist(c(-pars.u[,ka],0,0,
        ##                         pars.u[,ka],-(pars.u[,cl]+pars.u[,q])/pars.u[,v2],pars.u[,q]/pars.u[,v3],
        ##                         0,pars.u[,q]/pars.u[,v2],-pars.u[,q]/pars.u[,v3])),nrow=3,byrow=TRUE)
        Asys <- matrix(unlist(c(-pars.u[ka],0,0,
                                pars.u[ka],-(pars.u[cl]+pars.u[q])/pars.u[v2],pars.u[q]/pars.u[v3],
                                0,pars.u[q]/pars.u[v2],-pars.u[q]/pars.u[v3])),nrow=3,byrow=TRUE)

        thalves.u <- log(2)/-sort(eigen(Asys)$values)
        thalves.u
    })

    ths2 <- t(thalves)
    
    if(transform){
        colnames(ths2) <- paste("th",1:ncol(ths2),sep="")
        ths2 <- cbind(pars,ths2)
    }

    return(ths2)
}

### if pars is a data.table. Will be calculated for each row.
##' @import data.table
##' @import NMdata
thalf_1a2c_dt <- function(pars,cl="CL",ka="KA1",v2="V2",q="Q",v3="V3",transform=F,debug=F){
    if(debug) browser()
    pars <- copy(as.data.table(pars))
    
    varnames <- c(cl,ka,v2,q,v3)
    varsInPars <- varnames%in%names(pars)
    if(any(!varsInPars)){stop(paste("These variables are not in pars:",paste(varnames[!varsInPars],collapse=", ")))}

    setnames(pars,old=varnames,new=c("cl","ka","v2","q","v3"))
    
    rowcol <- NMdata::tmpcol(pars)
    pars[,(rowcol):=1:.N]
    thalves <- pars[,{
        Asys <- matrix(c(-ka,0,0,
                                ka,-(cl+q)/v2,q/v3,
                                0,q/v2,-q/v3),nrow=3,byrow=TRUE)
            
        thalves.u <- log(2)/-sort(eigen(Asys)$values)
        list(nth=1:length(thalves.u),thalf=thalves.u)
    }
         ,by=rowcol]
    
    

    ths2 <- dcast(thalves,as.formula(paste0(rowcol,"~nth")),value.var="thalf")
    ths2[,(rowcol):=NULL]    
    if(transform){
        colnames(ths2) <- paste("th",1:ncol(ths2),sep="")
        ths2 <- cbind(pars,ths2)
    }

    return(ths2[])
}




### This is only 1st order models, single-depot abs. One row of parameters only. Use thalf.1a.1c instead.
## thalf.1a.1c.one <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
##     if(debug)browser()
##     pars.u <- unique(pars[,c(cl,ka,v2)])
##     ##    print(pars.u[,cl])
##     Asys <- matrix(unlist(c(-pars.u[ka],0,pars.u[ka],-pars.u[cl]/pars.u[v2])),nrow=2,byrow=TRUE)
##     ##    eigen(Asys)
##     ## This is the true Half time - which equals the absorption half time
##     thalf.fast <- log(2)/-min(eigen(Asys)$values)
##     thalf.term <- log(2)/-max(eigen(Asys)$values)
##     thalf <- list(init=thalf.fast,term=thalf.term)
##     return(thalf)
## }




##' Thalf for 1 absorption depot, 1 compartment
##'
##' @description Can take multiple parameter sets and return a data.frame of halftimes.
##' @param pars A parameter table. Thalf will be calculated for all rows.
##' @param cl The name of the column with clearance values. Can represent CL/F if volume variables do the same.
##' @param ka The name of the column representing absorption rate.
##' @param v2 The name of the column with central volume (see cl as well).
##' @param debug Start by calling browser()?
##' @family Calc
thalf_1a1c <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
    if(debug)browser()
    ## the rest for each row
    pars$atempROW <- 1:nrow(pars)

    fun.thalfs <- function(x){
        Asys <- matrix(c(-x[[ka]],0,
                         x[[ka]],-x[[cl]]/x[[v2]]),nrow=2,byrow=TRUE)
        ##    eigen(Asys)
        ## This is the true Half time - which equals the absorption half time
        thalf.fast <- log(2)/-min(eigen(Asys)$values)
        thalf.term <- log(2)/-max(eigen(Asys)$values)
        thalf <- data.frame(atempROW=x$atempROW,thalf.init=thalf.fast,thalf.term=thalf.term)
        
        return(thalf)
    }

    thalfs <- do.call(rbind,by(pars,pars$atempROW,fun.thalfs))
    thalfs[,!colnames(thalfs)%in%"atempROW"]
    
}


##' Thalf for 1st order models, single-depot abs through any number of transit compartments.
##' @param pars A parameter table. Thalf will be calculated for all rows.
##' @param cl The name of the column with clearance values. Can represent CL/F if volume variables do the same.
##' @param ka The name of the column representing absorption rate.
##' @param vc The name of the column with central volume (see cl as well).
##' @param debug Start by calling browser()?
##' @family Calc
thalf_trans_1c <- function(pars,cl="CL",ka="KA1",vc="VC",debug=F){
    if(debug)browser()
    ## the rest for each row
    pars$atempROW <- 1:nrow(pars)
    Ntrans <- length(ka)
    NC <- length(vc)
    Nstates <- Ntrans+NC

    
    fun.thalfs <- function(x){
        Asys <- diag(c(-x[ka],-x[[cl]]/x[[vc]]))
        for(I in 1:Ntrans) Asys[I+1,I] <- x[[ka[[I]]]]

        ## This is the true Half time - which equals the absorption half time
        thalf.fast <- log(2)/-min(eigen(Asys)$values)
        thalf.term <- log(2)/-max(eigen(Asys)$values)
        thalf <- data.frame(atempROW=x$atempROW,thalf.init=thalf.fast,thalf.term=thalf.term)
        
        return(thalf)
    }

    thalfs <- do.call(rbind,by(pars,pars$atempROW,fun.thalfs))
    thalfs <- merge(pars,thalfs,by="atempROW")
    thalfs[,"atempROW"] <- NULL
    
}




##### 2 absorption depots (split depot), 2 compartments
### This one only works for one set of parameters. Has to be extended to handled
### several parameter sets and return a data frame with all the thalves for each
### line in pars.
##' @family Calc
thalf_2a2c <- function(pars,cl="CL",ka1="KA1",ka2="KA2",v2="V2",q="Q",v3="V3",debug=F){
    warning("This function is experimental. Please check the code.")
    if(debug)browser()
    
    varnames <- c(cl,ka1,ka2,v2,q,v3)
    varsInPars <- varnames%in%names(pars)
    if(any(!varsInPars)){stop(paste("These variables are not in pars:",paste(varnames[!varsInPars],collaps=", ")))}
    
    pars.u <- unique(pars[,c(cl,ka1,ka2,v2,q,v3)])
    ##    print(pars.u[,cl])
    Asys <- matrix(unlist(c(
        -pars.u[,ka1],0,0,0,
        pars.u[,ka1],-(pars.u[,cl]+pars.u[,q])/pars.u[,v2],pars.u[,q]/pars.u[,v3],pars.u[,ka2],
        0,pars.u[,q]/pars.u[,v2],-pars.u[,q]/pars.u[,v3],0,
        ## The second absorption depot
        0,0,0,-pars.u[,ka2])),
        nrow=4,byrow=TRUE)
    
    thalves <- log(2)/-sort(eigen(Asys)$values)
    
    return(thalves)
}

############ more derived parameters at once. 
###{
##' @family Calc
derivePars_1a1c <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
    if(debug)browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    atempROW <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
    ## the rest for each row
    pars$atempROW <- 1:nrow(pars)

    fun.thalfs <- function(x){
        Asys <- matrix(c(-x[[ka]],0,
                         x[[ka]],-x[[cl]]/x[[v2]]),nrow=2,byrow=TRUE)
        ##    eigen(Asys)
        ## This is the true Half time - which equals the absorption half time
        thalf.fast <- log(2)/-min(eigen(Asys)$values)
        thalf.term <- log(2)/-max(eigen(Asys)$values)
        thalf <- data.frame(atempROW=x$atempROW,thalf.init=thalf.fast,thalf.term=thalf.term)
        
        return(thalf)
    }

    thalfs <- do.call(rbind,by(pars,pars$atempROW,function(x)fun.thalfs(x)))
    res1 <- subset(merge(pars,thalfs,by="atempROW"),select=-atempROW)
    res1[,"MAT"] <- 1/res1[,ka]
    res1[,"MRT.IV"] <- res1[,v2]/res1[,cl]
    res1[,"MRT"] <- res1[,"MAT"]+res1[,"MRT.IV"]
    res1[,"tmax"] <- 1/(res1[,ka]-res1[,cl]/res1[,v2])*log(res1[,ka]*res1[,v2]/res1[,cl])
### for a unit dose
    res1[,"AUC"] <- 1/res1[,cl]
    res1[,"Cmax"] <- 1/res1[,v2]*exp(-res1[,cl]/res1[,v2]*res1[,"tmax"])
    res1
}
###}


##' Thalf for 1 absorption depot, 2 compartments
##'
##' @description Can take multiple parameter sets and return a data.frame of halftimes.
##' @param pars A parameter table. Thalf will be calculated for all rows.
##' @param cl The name of the column with clearance values. Can represent CL/F if volume variables do the same.
##' @param ka The name of the column representing absorption rate.
##' @param v2 The name of the column with central volume (see cl as well).
##' @param q The name of the column with intercompartmental clearance.
##' @param v3 The name of the column with peripharel volume (see cl as well).
##' @param transform Add results to existing dataset?
##' 
##' @param debug Start by calling browser()
## For thalf, I think this is the one to use. Would be good to generalise this to use for simpler systems.
##' @family Calc

thalf_1a2c_vec <- function(cl,ka,v2,q,v3,transform=F,debug=F){
    if(debug)browser()


    dt.pars <- data.table(cl=cl,ka=ka,v2,q=q,v3=v3)[
        ,ROW:=.I]
    
    thalves <- dt.pars[,{
                       Asys <- matrix(unlist(c(-ka,0,0,
                                ka,-(cl+q)/v2,q/v3,
                                0,q/v2,-q/v3)),nrow=3,byrow=TRUE)

        thalves.u <- log(2)/-sort(eigen(Asys)$values)
        thalves.u
    },by=ROW]

    ## thalves[,par:=rep(paste0("th",1:3),length(cl))]
    thalves[,par:=paste0("th",1:3),by="ROW"]
    ths2 <- dcast(thalves,ROW~par,value.var="V1")    
    ths2[,ROW:=NULL]
    ## ths2 <- t(thalves)
    
    ## if(transform){
    ##     colnames(ths2) <- paste("th",1:ncol(ths2),sep="")
    ##     ths2 <- cbind(pars,ths2)
    ## }

    return(ths2[])
}
