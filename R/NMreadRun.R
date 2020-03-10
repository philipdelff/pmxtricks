
##' read model details
##'
##' This function is used to produce run logs or run records by NMrunLog. Much better alternatives can most likely be found elsewhere.
##' @param run path to an lst file.
##' @param debug Start by calling browser()?
##' @family Nonmem
##' @import nonmem2R


## for inspiration, from Pirana design:
## "003" = list (
## " modelfile " = "003. mod " ,
## " description " = " PK model digoxin " ,
## " reference_model " = "002" ,
## " data_file " = " nm_pk_001 . csv " ,
## " output_file " = "003. lst " ,
## " tables " = c ("003. TAB " , " sdtab003 ")
## )

## TODO Add dataset name
## TODO add timestamp
## TODO If lst does not exist, take what you can from .mod
## TODO get shrinkage info


NMreadRun <- function(run,debug=F){
    if(debug) browser()

    ## for now we can assume run is a lst file
    ## runbase <- sub("\\.lst","",run)
    dir <- dirname(run)
    run <- sub("^ ","",run)
    run <- sub(" $","",run)
    runname <- sub("\\.lst$","",basename(run))
    runname <- sub("\\.lst","",basename(run))
    file.lst <- filePathSimple(dir,paste0(runname,".lst"))
    file.mod <- filePathSimple(dir,paste0(runname,".mod"))
    file.ext <- file.path(dir,paste0(runname,".ext"))
    
    lstOrMod <- ""
    if(file.exists(file.lst)) {
        lstOrMod <- "lst"
        runfile <- file.lst
    } else if(file.exists(file.mod)) {
        lstOrMod <- "mod"
        runfile <- file.mod
    } else {
        stop("neither .mod nor .lst exist")
    }

    if(lstOrMod=="lst"){
        n2r.sumo <- sumofork(file.lst)
        out <- n2r.sumo
    } else {
        out <- list()
    }
    
    out$run=runname
    problem <- NMgetSection(runfile
                           ,section="PROBLEM"
                           ,keepName=F
                           ,keepEmpty=F
                           ,keepComments=F
                           ,cleanSpaces=T
                            )

    out$problem <- problem


    ## find reference as in ";; Reference modname"
    lines.lst <- readLines(runfile)
    run.ref <- NA_character_
    line.ref <- grep("^[ ;]*; *reference",x=lines.lst,ignore.case=T)
    if(length(line.ref)==1){
        run.ref <- sub("^[ ;]*; *reference.* ([^ ]*) *","\\1",lines.lst[line.ref],ignore.case=T)
    }
    out$run.ref <- run.ref

    npars.symmat <- function(mat) {nrow(mat)+ ## the diagonal
                                       (length(mat)-nrow(mat))/2 ## off diag
    }
    
### no of parameters - this is broken in extload. Has to be implemented differently
    ## if(file.exists(file.ext)){
    ##     n2r.ext <- extload(file.ext)
    ##     nfix.theta <- sum(grepl("^THETA",n2r.ext$fix))
    ##     s1 <- strsplit( n2r.ext$fix,"\\.")
    ##     browser()
    ##     n.diags <- sum(sapply(s1,function(x) x[1]%in%c("OMEGA","SIGMA") && x[2]==x[3] ))
    ##     n.off.diags <- sum(sapply(s1,function(x) x[1]%in%c("OMEGA","SIGMA") && x[2]!=x[3] ))
    ##     n.pars.total <- length(c(n2r.ext$omega,n2r.ext$theta,n2r.ext$sigma))
    ##     n.fix <- nfix.theta+n.diags+2*n.off.diags
    ##     n.pars.est <- n.pars.total-n.fix
    ##     out$Npars <- n.pars.est
    ## } else {
    ##     out$Npars <- NA
    ## }

### Minimization succesful
    out$min.success <- any(grepl("^0MINIMIZATION SUCCESSFUL",lines.lst))
### problems with minimization
    out$min.problem <- any(grepl("^ HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.",lines.lst))
    
### Parameter near boundary
    out$near.bound <- any(grepl("^0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY",lines.lst))

    ## NMgetSection2(file=file.models("BaseModel/run7162.lst"),section="ITERATION",char.section="0",type="res",debug=F)
### find last gradients
    its=NMextractText(lines=lines.lst,section="ITERATION",char.section="0",type="res",debug=F)
    last.grad=its[max(grep("^ *GRADIENT",its))]
    last.grad <- sub("^ *GRADIENT: *","",last.grad)
    last.grad.n <- read.table(text=last.grad,header=F)[1,]

### Max gradient
    out$max.grad <- abs(as.numeric(last.grad.n[which.max(abs(last.grad.n))]))

    
### zero gradient
    out$gradient.zero <- any(last.grad.n==0)

### a summary of convergence
    convsum <- ifelse(out$min.success,"OK","Minimization failed")
    
    if(convsum=="OK"){
        if(any(out$min.problem,out$near.bound,out$gradient.zero)){
            convsum <- ""
            if(out$min.problem) convsum <- paste0(convsum,"Problems. ")
            if(out$near.bound) convsum <- paste0(convsum,"Near bound. ")
            if(out$gradient.zero) convsum <- paste0(convsum,"Zero gradient. ")
            
            if(convsum=="Problems. ") convsum <- paste0(convsum,"max.grad=",out$max.grad)
        }
    }
    out$convsum <- convsum

    

    
    covinfo <- data.table(path.full.lst  = file.lst)
    covinfo[,fn.lst:=basename(path.full.lst)]
    ## covinfo[,path.full :=  file.models("BaseModel",fn.lst)]
    covinfo[,ROW := 1:.N]
    covinfo[,text.lst:= paste(readLines(path.full.lst),collapse="\n"),by = .(ROW)]

    covinfo[,cov.request:= ifelse(is.null(NMgetSection(text = text.lst,section  =  "COV",keepName  = T,debug=F)),"No","Yes"),by = .(ROW)]

    covinfo[,sec.cov:= paste(NMgetSection(text =  text.lst,section  =  "COV",keepName  =  T),collapse = "\n" ),by = .(ROW)]

    covinfo[grepl("MATRIX",sec.cov),
            cov.request := sub(".*MATRIX *= *([A-Z]*).*","\\1",sec.cov)]

    ##    covinfo[,cov.abort:=grepl("0COVARIANCE STEP ABORTED",paste(text.lst))]

    
    covinfo[,cov.comment:=""]
    covinfo[cov.request=="No",cov.comment:=NA_character_]

    covinfo[cov.request!="No"&grepl("Elapsed covariance time in seconds",text.lst),cov.comment:="Success"]
    covinfo[cov.request!="No"&grepl("0COVARIANCE STEP ABORTED",paste(text.lst)),cov.comment:="Aborted"]
    covinfo[cov.request!="No"&grepl("0R MATRIX ALGORITHMICALLY SINGULAR",text.lst),cov.comment:=paste(cov.comment,"R singular.")]
    covinfo[cov.request!="No"&grepl("0R MATRIX ALGORITHMICALLY NON-POSITIVE-SEMIDEFINITE",text.lst),cov.comment:=paste(cov.comment,"R non-pos semidef.")]
    
    covinfo[cov.request!="No"&grepl("0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY",text.lst),cov.comment:=paste(cov.comment,"Near boundary.")]


    ## remove text.lst before exporting
    covinfo[,text.lst:=NULL]

    out$cov.request <- covinfo[,cov.request]
##    out$cov.abort <- covinfo[,cov.abort]
    out$cov.comment <- covinfo[,cov.comment]
    
    out$covsum <- covinfo[,.(covsum=paste(cov.request,cov.comment,sep=" - ")),by=.(ROW)]$covsum
    out$conv.OK <- out$minSuccessful&!out$roundingErrors&out$finalZeroGradient==0&out$covSuccessful
    
    if(file.exists(file.ext)){
        n2r.ext <- extload(file.ext)
        n.pars.est <- with(n2r.ext,npars.symmat(sigma)+length(theta)+npars.symmat(omega)-length(fix))
        out$Npars <- n.pars.est
    } else {
        out$Npars <- NA
    }


    out2 <- newNames(out,names=data.frame(old=c("totNoOfIndividuals" ,"totNoOfObservations"),new=c("Nsubjs","Nobs")),debug=F)

    out2

}
