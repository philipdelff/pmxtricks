s### most of these functions are t1/2 functions. But functions to calculate more derived parameters follow.

### This is only 1st order models, single-depot abs. One row of parameters only. Use thalf.1a.1c instead.
thalf.1a.1c.one <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
    if(debug)browser()
    pars.u <- unique(pars[,c(cl,ka,v2)])
    ##    print(pars.u[,cl])
    Asys <- matrix(unlist(c(-pars.u[ka],0,pars.u[ka],-pars.u[cl]/pars.u[v2])),nrow=2,byrow=TRUE)
    ##    eigen(Asys)
    ## This is the true Half time - which equals the absorption half time
    thalf.fast <- log(2)/-min(eigen(Asys)$values)
    thalf.term <- log(2)/-max(eigen(Asys)$values)
    thalf <- list(init=thalf.fast,term=thalf.term)
    return(thalf)
}




### This is only 1st order models, single-depot abs.
thalf.1a.1c <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
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
    ## select(merge(pars,thalfs,by="atempROW"),-atempROW)
    thalfs[,!colnames(thalfs)%in%"atempROW"]
   
}


### This is only 1st order models, single-depot abs through abs transit compartments.
thalf.trans.1c <- function(pars,cl="CL",ka="KA1",vc="VC",debug=F){
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
    select(merge(pars,thalfs,by="atempROW"),-atempROW)
   
}


    
##### 1 absorption depot, 2 compartments
### This one only works for one set of parameters. Has to be extended to handled several parameter sets and return a data frame with all the thalves for each line in pars.
thalf.1a.2c.single <- function(pars,cl="CL",ka="KA1",v2="V2",q="Q",v3="V3",debug=F){
    warning("use thalf.1a.2c instead")
    if(debug)browser()
    pars.u <- unique(pars[,c(cl,ka,v2,q,v3)])

    Asys <- matrix(unlist(c(-pars.u[,ka],0,0,
                            pars.u[,ka],-(pars.u[,cl]+pars.u[,q])/pars.u[,v2],pars.u[,q]/pars.u[,v3],
                            0,pars.u[,q]/pars.u[,v2],-pars.u[,q]/pars.u[,v3])),nrow=3,byrow=TRUE)
        
    thalves <- log(2)/-sort(eigen(Asys)$values)
    
    return(thalves)
}

##' 1 absorption depot, 2 compartments
##'
##' Can take multiple parameter sets and return a data.frame of halftimes.
thalf.1a.2c <- function(pars,cl="CL",ka="KA1",v2="V2",q="Q",v3="V3",mutate=F,debug=F){
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
    
    if(mutate){
        colnames(ths2) <- paste("th",1:ncol(ths2),sep="")
        ths2 <- cbind(pars,ths2)
    }

    return(ths2)
}


thalf.1a.2c.plyr <- function(pars,cl="CL",ka="KA1",v2="V2",q="Q",v3="V3",debug=F){
    warning("use thalf.1a.2c instead")
    require(plyr)
    
    if(debug)browser()
    ## pars.u <- unique(pars[,c(cl,ka,v2,q,v3)])

    varnames <- c(cl,ka,v2,q,v3)
    varsInPars <- varnames%in%names(pars)
    if(any(!varsInPars)){stop(paste("These variables are not in pars:",paste(varnames[!varsInPars],collaps=", ")))}
    
    
    thalves <- apply(pars,1,function(pars.u){
            Asys <- matrix(unlist(c(-pars.u[ka],0,0,
                                pars.u[ka],-(pars.u[cl]+pars.u[q])/pars.u[v2],pars.u[q]/pars.u[v3],
                                0,pars.u[q]/pars.u[v2],-pars.u[q]/pars.u[v3])),nrow=3,byrow=TRUE)

        
        
            thalves.u <- log(2)/-sort(eigen(Asys)$values)
            thalves.u
    })
    
    return(t(thalves))
}




##### 2 absorption depots (split depot), 2 compartments
### This one only works for one set of parameters. Has to be extended to handled
### several parameter sets and return a data frame with all the thalves for each
### line in pars.
thalf.2a.2c <- function(pars,cl="CL",ka1="KA1",ka2="KA2",v2="V2",q="Q",v3="V3",debug=F){
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

############ more derived parameters at once
###{
derivePars.1a.1c <- function(pars,cl="CL",ka="KA1",v2="V2",debug=F){
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
