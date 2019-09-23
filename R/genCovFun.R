##' create R function that generates param values based on covariates
##' @description This function is really useful bu this and a few
##'     other may belong better in a simulation package.
##' @param NMcode0 Nonmem code that translates THETA and OMEGA into
##'     PK/PD parameters like CL, V2, EMAX etc.
##' @param pars0 The names of the final individual parameters that the
##'     generated function must return and which can be used for
##'     simulations.
##' @param covs0 Names of covariates which are arguments to the generated function.
##' @param theta0 default theta for resulting function.
##' @param omega0 default omega for resulting function.
##' @param name.fun The name of the function to be generated.
##' @param debug Start by calling browser()?
##' @return A character string which can be evaluated to create a function. 
##' @details The function returns a function in plain text. If you
##'     want to store the code, paste this text to a file. If you want
##'     to use it right away, do fun1text <- genCovFun() newfun <-
##'     eval(parse(text=fun1text))
##' @family Simulation
##' @examples
##' \dontrun{
##' text0 <- NMgetSection(file="run1.lst",name="PK",return="text",keepName=FALSE,cleanSpaces=FALSE)
##' 
##' NMR0 <- NMcode2R(text0)
##' ## get rid of some text that has nothing to do with covariates
##' NMR1 <- NMR0[-c(1:21)]
##' NMR2 <- NMR1[1:25]
##' 
##' pars <- extload(paste0(run,".ext"))
##' 
##' fun1text <- genCovFun(NMcode0=NMR3
##'                      ,pars0=c("KA","DCL","V2","V3","Q","D1","F1","CL0","KCL")
##'                      ,theta0=pars$theta
##'                      ,omega0=pars$omega
##'                      ,name.fun="parsBase"
##'                       ## ,debug=T
##'                       )
##' 
##' cat(fun1text)
##' }


## dont export from pmxtricks. Needs a little more testing. Maybe to be moved to pmxsim package?

## TODO 
## In case the resulting function doesn't take any covariates (covs <- NULL) the resulting code could be simplified. It seems overly complicated to have a loop over covs when covs is NULL. Should the df.covs argument even be included if none are used anyway?
## TODO end

genCovFun <- function(NMcode0,pars0,covs0,theta0,omega0,name.fun,debug=F){
    if(debug) browser()

    text.name <- ifelse(missing(name.fun),"",paste0(name.fun," <- "))
    lines <- paste0(text.name,"function(df.cov,col.id=\"ID\",Nnew,theta,omega,debug=F){

    if(debug) browser()
")

    if(missing(covs0)){
        lines <- paste0(lines,"
covs <- NULL
")
    } else {
lines <- paste0(lines,"
covs <- list(",paste(covs0,collapse=","),")
names(covs) <- c(\"",paste0(names(covs0),collapse="\",\""),"\")
")
}
    lines <- paste0(lines," names.pars <- c(\"",
                    paste0(pars0,collapse="\",\""),"\")
NMcode <- \"",paste(NMcode0,collapse="\n"),"\"

    if(missing(df.cov)) {
        message(\"df.cov not supplied. One subject will be generated.\")
        df.cov <- data.frame(id=1)
        colnames(df.cov) <- col.id
    }

    if(!col.id%in%names(df.cov)) {
        df.cov[,col.id] <- 1:nrow(df.cov)
    }
    n.covs <- length(covs) 
    names.covs <- names(covs)

    if(n.covs>0){
        for(I in 1:n.covs){
            if(is.null(df.cov[[names.covs[I]]])) {
                df.cov[,names.covs[I]] <- covs[[I]][1]
            } else {
                df.cov[is.na(df.cov[[names.covs[I]]]),names.covs[I]] <- covs[[I]][1]
                ## if a set of allowed values is given. Should return a better error msg.
                if(length(covs[[I]])>1) {
                    vals.allowed <- unique(c(covs[[I]][[1]],covs[[I]][[2]]))
                    if(!all(df.cov[,names.covs[I]]%in%vals.allowed)){
                        browser()
                        stop(\"wrong covariate value\")
                    }
                }
            }
        }
    }

    if(missing(theta)) theta <- c(",paste(theta0,collapse=","),")
    THETA <- theta
    if(missing(omega)) omega <- matrix(c(",
paste(c(omega0),collapse=","),"),nrow=",sqrt(length(omega0)),")
    OMEGA <- omega
    NETAS <- nrow(OMEGA)

### random variability not implemented
    if(!missing(Nnew)){
        library(MASS)

        if(Nnew<1) stop(\"Nnew must be larger than 0\")
        if(nrow(df.cov)>1&Nnew>0) stop(\"When Nnew>0, only one set of covariates or one typical subject can be used.\")
        if(nrow(df.cov)>0) Nsim <- nrow(df.cov)
        if(Nnew>1) {
            message(\"New subjects are being generated. New subject IDs generated.\")
            Nsim <- Nnew
            df.cov <- df.cov[rep(1,Nsim),,drop=F]    
            df.cov[,col.id] <- 1:Nsim
        }

        ETAS <- mvrnorm(n=Nsim,mu=rep(0,NETAS),Sigma=omega)
        if(Nsim==1) ETAS <- matrix(ETAS,nrow=1)

        df.ETAS <- as.data.frame(ETAS,col.names=paste0(\"ETA\",1:NETAS))
        colnames(df.ETAS) <- paste0(\"ETA\",1:NETAS)
     } else {

         m1 <- gregexpr(\"[^A-Za-z]ETA\\\\[[[:digit:]]+\\\\]\",NMcode)
         etas.char <- sub(\".ETA\",\"ETA\",do.call(c,regmatches(NMcode,m1)))
         etas.n <- as.numeric(sub(\"ETA\\\\[([[:digit:]])\\\\]\",\"\\\\1\",etas.char))
         ## ETA <- rep(0,max(etas.n))
         ##browser()
         NETAS <- max(etas.n)
         ETAS = matrix(rep(0,nrow(df.cov)*NETAS),ncol=NETAS)
         df.ETAS <- as.data.frame(ETAS)
         colnames(df.ETAS) <- paste0(\"ETA\",1:NETAS)
     }

     df.cov <- cbind(df.cov,df.ETAS)


     l.cov <- list()
     for(I in 1:nrow(df.cov)) l.cov[[I]] <- df.cov[I,,drop=F]

     rows.pars.l <- lapply(l.cov,function(row.cov){
         row.pars <- with(row.cov,{
                ## browser()
                ETA <- row.cov[paste0(\"ETA\",1:NETAS)]
                eval(parse(text=NMcode))
  
                pars.and.id <- c(col.id,names.pars)
                pars.and.id <- as.data.frame(lapply(pars.and.id,get,envir=sys.frame(sys.parent(0))))
                names(pars.and.id) <- c(col.id,names.pars)
                pars.and.id
                }
                )
            })

        df.par <- do.call(rbind,rows.pars.l)
        names(df.par) <- c(col.id,names.pars)
        df.all <- cbind(df.cov,df.par)
        return(list(pars=df.par,covs=df.cov,all=df.all))
}
")
    cat(lines)
    return(lines)
}
