
## run <- "\\\\samba-hpc.rd.astrazeneca.net\\QCP_MODELING\\ONC\\azd5363\\poppk_20180813_Ph1Ph2Pooled_final\\Models\\baseModel\\philipdelff\\run416t14_cn_01.lst"

## for inspiration, from Pirana design:
## "003" = list (
## " modelfile " = "003. mod " ,
## " description " = " PK model digoxin " ,
## " reference_model " = "002" ,
## " data_file " = " nm_pk_001 . csv " ,
## " output_file " = "003. lst " ,
## " tables " = c ("003. TAB " , " sdtab003 ")
## )

## **** TODO Add dataset name
## **** TODO Add output tables
## TODO add timestamp
## TODO If lst does not exist, take what you can from .mod



NMreadRun <- function(run){
    library(nonmem2R)
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
                           ,name="PROBLEM"
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
    out$Npars <- NA

    out2 <- pmxtricks::newNames(out,names=data.frame(old=c("totNoOfIndividuals" ,"totNoOfObservations"),new=c("Nsubjs","Nobs")),debug=F)

    out2

}
