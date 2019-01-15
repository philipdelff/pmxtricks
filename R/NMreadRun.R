
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


NMreadRun <- function(run){
    library(nonmem2R)
    ## for now we can assume run is a lst file
    ## runbase <- sub("\\.lst","",run)
    dir <- dirname(run)
    runname <- sub("\\.lst","",basename(run))
    file.lst <- filePathSimple(dir,paste0(runname,".lst"))

    n2r.sumo <- sumoR(file.lst)

    out <- n2r.sumo
    out$run=runname
    problem <- NMgetSection(file.path(dir,paste0(runname,".lst"))
                           ,name="PROBLEM"
                           ,keepName=F
                           ,keepEmpty=F
                           ,keepComments=F
                           ,cleanSpaces=T
                            )

    out$problem <- problem
    

    ## find reference as in ";; Reference modname"
    lines.lst <- readLines(file.lst)
    run.ref <- NA_character_
    line.ref <- grep("^[ ;]*; *reference",x=lines.lst,ignore.case=T)
    if(length(line.ref)==1){
        run.ref <- sub("^[ ;]*; *reference.* ([^ ]*) *","\\1",lines.lst[line.ref],ignore.case=T)
    }
    out$run.ref <- run.ref
    
    ## no of parameters
    n2r.ext <- extload(file.path(dir,paste0(runname,".ext")))
    nfix.theta <- sum(grepl("^THETA",n2r.ext$fix))
    s1 <- strsplit( n2r.ext$fix,"\\.")
    n.diags <- sum(sapply(s1,function(x) x[1]%in%c("OMEGA","SIGMA") && x[2]==x[3] ))
    n.off.diags <- sum(sapply(s1,function(x) x[1]%in%c("OMEGA","SIGMA") && x[2]!=x[3] ))
    n.pars.total <- length(c(n2r.ext$omega,n2r.ext$theta,n2r.ext$sigma))
    n.fix <- nfix.theta+n.diags+2*n.off.diags
    n.pars.est <- n.pars.total-n.fix
    
    out$Npars <- n.pars.est

    out2 <- pmxtricks::newnames(out,names=data.frame(old=c("totNoOfIndividuals" ,"totNoOfObservations"),new=c("Nsubjs","Nobs")),debug=F)
    
    out2
    
}
