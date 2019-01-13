##' automatically find Nonmem tables and organize data

#### change log
### NMscan7 -> NMscan8
## automatic detection of psn/QCP structure.

## adding possibility to stack with discarded lines from input data.

### NMscan5 -> NMscan6

## replaced a few plyr calls and changed function calls to comply with QCPfunc 2.0.1

## checks if inpu file exists. If not, it just outputs the output data.

#### end change log


### todo
## bug: skip input data if not found.

## exit if no tables are found

## use default values for col.grp and col.occ. Use if present.

### end todo 


##' @param col.grp If present, ID and OCC level info is grouped by col.grp. So should only be needed for cross-over.

source("E:/Users/ppda/R_functions_ppda/findCovs.R")

NMscan <- function(path.run,col.id="ID",col.row="ROW",col.grp=NULL,col.occ="OCC",structure="full",use.input=T,reconstructRows=F,debug=F){
    if(debug) browser()

    ### at least plyr::arrange is used
    library(plyr)


###{ process arguments 
    path.run <- file.path.simple(path.run)
    if(!file.exists(path.run)) stop("path.run does not exist.")
    if(file.info(path.run)$isdir){
        file.model <- file.path(path.run,"output.txt")
        if(!file.exists(file.model)) stop("path.run is a directory but does not contain output.txt")
    } else {
        file.model <- path.run
        if(sub(".*\\.(.*)$","\\1",basename(path.run))=="mod"){
            ## file.model <- sub("(.*\\.)(.*)$","\\1lst",path.run)
            file.model <- path.run
            ## if(!file.exists(file.model)) stop("path.run is a file ending in .mod so assuming that output file from nonmem is to be found in corresponding .lst. This file is however missing. Model not run?")
        }
        path.run <- dirname(path.run)
    }
    ## file.output <- switch(str.nm,
    ##                       QCP=file.path(path.run,"output.txt"),
    ##                       psn=path.run)
    ## output <- readLines(file.output)
    ## if(str.nm=="psn") {
    ##     path.run <- QCPfunc:::file.path.simple(path.run)
    ##     path.run <- dirname(path.run)
    ## }

###}

###{ read output text from Nonmem
    ## we call this output. But in psn case, this is actually the input (mod) file.
    output <- readLines(file.model)
    ## discard comments
    output <- output[!grepl("^ *;.*",output)]

    ## discard output part - only keep the control stream
    
    ##### find all tables in output
    ## lines.tab <- output[findDollar(output,name="TABLE")]
    ## this requires QCPfunc 2.0.0
    lines.tab <- output[QCPfunc:::findDollar(output,name="TABLE")]
    
    lines.tab <- lines.tab[1:(min(c(length(lines.tab),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines.tab)-1)))]
    ##lines.tab <- lines.tab[1:(min(grep("NM-TRAN MESSAGES",lines.tab))-1)]
    nlines.tabdollars <- grep("\\$TABLE",lines.tab)
    nlines.tabfiles <- grep("FILE *=",lines.tab)
    stopifnot(length(nlines.tabdollars)==length(nlines.tabfiles))
    stopifnot(all(nlines.tabdollars<=nlines.tabfiles))

##### find the all rows tables - they will be merged into one.
    ## lines.tab
    tablines.l <- strsplit(paste(lines.tab,collapse=" "),split="\\$TABLE")[[1]]
    tablines.l <- tablines.l[-1]
    overview.tables <- rbindUnion(lapply(tablines.l,function(x){
        data.frame(
            file=sub(".*FILE=([^ ]*).*","\\1", x),
#### these are valid as well.
### FIRSTRECORDONLY or FIRSTRECONLY
            firstonly=grepl("FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY",x)
        )
    }))
###}

###{ read all output tables and merge to max one firstonly and max one row
    data <- lapply(overview.tables$file,function(x){
        out <- NMimport(QCPfunc:::absPath(file.path(path.run,x)),silent=T)
        out$TABLE <- NULL
        out
    })


    overview.tables <- cbind(overview.tables,setNames(as.data.frame(do.call(rbind,lapply(data,function(x)c(dim(x),col.row%in%names(x))))),c("nrow","ncol","has.col.ROW")))
    overview.tables <- mutateQCP(overview.tables,maxLength=nrow==max(nrow))

    overview.tables <- mutateQCP(overview.tables,full.length=!firstonly&maxLength&has.col.ROW)
    overview.tables

    ## browser()
    
### combine full tables into one
    tabs.row <- which(overview.tables$full.length)
    all.row <- NULL
    if(length(tabs.row)){
        all.row <- setNames(
            data.frame(col.row=data[[tabs.row[1]]][,col.row])
           ,col.row)
        for(I in tabs.row){
            all.row <- merge(all.row,data[[I]][,c(col.row,setdiff(names(data[[I]]),names(all.row)))])
        }
    }

### combine firstonly tables into one
    tabs.firstonly <- which(overview.tables$firstonly)
    all.firstonly <- NULL
    if(length(tabs.firstonly)){
        all.firstonly <- setNames(
            data.frame(col.id=data[[tabs.firstonly[1]]][,col.id])
           ,col.id)
        for(I in tabs.firstonly){
## mergeCheck?
            all.firstonly <- merge(all.firstonly,data[[I]][,c(col.id,setdiff(names(data[[I]]),names(all.firstonly)))])
        }
    }
    
    data2 <- data[-c(tabs.row,tabs.firstonly)]
    data <- c(data2,list(all.row),list(all.firstonly))
    
    
###### all row tables combined into one
###}


###{ split tables into row, id, and occ level
### for each table
    ## scan for covariates
    ## scan for occasion variables
    ## check if col.row is present. If so, look for row-level info

    coltypes <- lapply(1:length(data),function(I){
        dat <- data[[I]]
        cnames <- colnames(dat)
        cnames.left <- cnames
        
        tab.id <- NULL
        if(col.id%in%cnames.left){
            covs.maybe <- findCovs(dat[,cnames.left],cols.id=c(col.id,col.grp))
            vars <- setdiff(names(covs.maybe),c(col.row,col.id,col.occ,col.grp,"CWRES","RES","WRES","PRED","IPRE","DV"))
            if(length(vars)){
                tab.id <- data.frame(type="id",table=I,var=vars,stringsAsFactors=F)
                cnames.left <- setdiff(cnames.left,vars)
            }
        }

        tab.occ <- NULL
        if(all(c(col.id,col.occ)%in%cnames.left)){
            covs.maybe <- findCovs(dat[,cnames.left],cols.id=c(col.id,col.occ,col.grp))
            vars <- setdiff(names(covs.maybe),c(col.row,col.id,col.occ,col.grp,"CWRES","RES","WRES","PRED","IPRE","DV"))
            if(length(vars)){
                tab.occ <- data.frame(type="occ",table=I,var=vars,stringsAsFactors=F)
                cnames.left <- setdiff(cnames.left,vars)
            }
        }
        
        tab.row <- NULL
        if(length(cnames.left)){
            vars <- setdiff(cnames.left,c(col.row,col.id,col.grp))
            tab.row <- data.frame(type="row",table=I,var=vars,stringsAsFactors=F)
            cnames.left <- setdiff(cnames.left,vars)
        }
        
        rbind(tab.row,tab.id,tab.occ)
    }
    )
    
    coltypes <- do.call(rbind,coltypes)
###}
    
###### now compare between tables and combine into one classsification of variables
    ## row-level and occ-level vars are only accepted from tables with most rows
    dims <- setNames(as.data.frame(do.call(rbind,lapply(data,dim))),c("nrow","ncol"))
    dims <- mutateQCP(dims,table=1:nrow(dims))
    coltypes <- merge(coltypes,dims,all.x=T)
    coltypes <- mutateQCP(coltypes,maxLength=nrow==max(nrow))
    coltypes <- subset(coltypes,!(type%in%c("row","occ")&!maxLength))

    ## first look at tables with max rows. Keep variables in only one of them. This is discarding redundant info.
    ## dismiss anything in tables with non-max nrows if it is present in a maxrow table    
    ## dismiss redundant info in non-max row tables.
    coltypes <- plyr::arrange(coltypes,type,-maxLength,var)

    coltypes <- coltypes[!duplicated(coltypes[,c("type","var")]),]

### dismiss overlapping with priority: row, occ, id.
    coltypes <- mutateQCP(coltypes,type.tmp=factor(type,levels=c("row","occ","id")))
    coltypes <- plyr::arrange(coltypes,type.tmp)
    coltypes <- coltypes[!duplicated(coltypes[,c("var")]),]
    coltypes$type.tmp <- NULL

    
    
### create output data.frames
    coltypes
    
    ## row-level
    tab.row <- NULL
    if(any(coltypes$type=="row")){
        tab.row <- unique(do.call(rbind,
                                  lapply(data,function(x) {
                                      if(all(c(col.row,col.id,col.grp)%in%names(x))){
                                          x[,c(col.row,col.id,col.grp),drop=F]
                                      } else {
                                          NULL
                                      }
                                  })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="row"])
        for(I in tabs.to.merge){
            tab.row <-
                merge(tab.row,data[[I]][,c(col.row,subset(coltypes,table==I&type=="row")$var),drop=F],by=col.row)
        }
    }

    ## subject-level
    tab.id <- NULL
    if(any(coltypes$type=="id")){
        tab.id <- unique(do.call(rbind,
                                 lapply(data,function(x) {
                                     if(all(c(col.id,col.grp)%in%names(x))){
                                         x[,c(col.id,col.grp),drop=F]
                                     } else {
                                         NULL
                                     }
                                 })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="id"])

        for(I in tabs.to.merge){
            tab.id <-
                merge(tab.id,
                      unique(data[[I]][,c(col.id,col.grp,subset(coltypes,table==I&type=="id")$var),drop=F]),by=c(col.id,col.grp))
        }
    }
    
    ## occ-level
    tab.occ <- NULL
    if(any(coltypes$type=="occ")){
        tab.occ <- unique(do.call(rbind,
                                  lapply(data,function(x) {
                                      if(all(c(col.id,col.occ,col.grp)%in%names(x))){
                                          x[,c(col.id,col.occ,col.grp),drop=F]
                                      } else {
                                          NULL
                                      }
                                  })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="occ"])
        for(I in tabs.to.merge){
            tab.occ <-
                merge(tab.occ,
                      unique(data[[I]][,c(col.id,col.occ,col.grp,subset(coltypes,table==I&type=="occ")$var),drop=F]),by=c(col.id,col.occ,col.grp))
        }
    }
    
    if(structure=="full"){
        
        ## all merged
        tab.all <- data.frame(toremove=1)
        if(!is.null(tab.row)) tab.all <- merge(tab.all,tab.row)
        if(!is.null(tab.id)) tab.all <- merge(tab.all,tab.id)
        if(!is.null(tab.occ)) tab.all <- merge(tab.all,tab.occ)
        tab.all$toremove <- NULL
        tab.row <- tab.all

        ## put id level vars on occ
        if(!is.null(tab.occ)&&!is.null(tab.id)){
            tab.occ.id <- merge(tab.occ,tab.id,by=c(col.id,col.grp),all.x=T)
            tab.occ <- tab.occ.id
        }
        
    }


    if(use.input){
        ## browser()

        ## lines.data <- output[findDollar(output,name="DATA")]
        ## requires QCPfunc 2.0.0 
        lines.data <- output[QCPfunc:::findDollar(output,name="DATA")]
        ## pick $DATA and the next string
        lines.data2 <- paste(lines.data,collapse=" ")
        lines.data3 <- sub(" *\\$DATA +","",lines.data2)
        lines.data4 <- sub("^file[ =]+","",lines.data3)
        lines.data5 <- sub(" .*","",lines.data4)
        path.data.input <- lines.data5

        pathIsAbs <- function(path) grepl("^/",path)
        if(pathIsAbs(path.data.input)){
### assuming we are on windows
            path.data.input <- QCPfunc:::absPath(path.data.input)
        } else {
            path.data.input <- file.path(path.run,path.data.input)
        }
        path.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",path.data.input)
        if(file.exists(path.data.input.rds)){
            message("found rds input file. This will be used.")
            path.data.input <- path.data.input.rds
            data.input <- readRDS(path.data.input)
        } else {
            if(file.exists(path.data.input)){
                message("Found input data file. Reading with read.csv")
                data.input <- read.csv(path.data.input,header=T,na.strings=".",stringsAsFactors=F)
            } else {
                warning(paste("Input data file not found. Only output data will be available. Was expecting to find",path.data.input))
                use.input <- FALSE
            }
        }
        
    }

    if(use.input){
        cnames.in <- colnames(data.input)
                
        if(!is.null(tab.row)){
            ## browser()
            data.output <- tab.row
            cnames.out <- colnames(data.output)
            ## we need col.row
            stopifnot(col.row%in%cnames.in)
            stopifnot(col.row%in%cnames.out)

            vars.to.recover <- cnames.in[!cnames.in%in%cnames.out]
            dim.out.0 <- dim(data.output)
            data.output.out <- merge(data.output,data.input[,unique(c(col.row,vars.to.recover))],by=col.row,all.x=T)
            dim.out.1 <- dim(data.output.out)
            stopifnot(all((dim.out.0+c(0,length(vars.to.recover)))==dim.out.1))
            data.output.out$nmout <- TRUE

            tab.row <- data.output.out[order(data.output.out[,col.row]),]
            
        }

        fun.merge <- function(data.in,data.out,cols.by,debug=F){
            if(debug) browser()
            cnames.in <- colnames(data.in)
            cnames.out <- colnames(data.out)
            ## if variables are in both in and output, output is preferred
            stopifnot(all(cols.by%in%colnames(data.in)))
            vars.common <- intersect(cnames.in,cnames.out)
            if(length(vars.common)){
                warning("common variables in input and output. Only the ones from output are kept.")
            }
            vars.to.recover <- cnames.in[!cnames.in%in%cnames.out]
            cnames.in.no.id <- cnames.in[!cnames.in%in%cols.by]
            ## if these are present in input, they will be dropped
            cnames.drop <- c("DV","PRED","RES","WRES")
            
            cnames.to.use <- vars.to.recover[!vars.to.recover%in%cnames.drop]
            Nid <- nrow(unique(data.in[,cols.by,drop=FALSE]))
            names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data.in[,c(cols.by,x)]))==Nid))]
            ## browser()
            reduced.in <- unique(data.in[,c(cols.by,names.covs),drop=F])
            ## browser()

            ## and possibly reduce the output as well (e.g. row could easily be present)
            Nid.out <- nrow(unique(data.out[,cols.by,drop=FALSE]))
            names.out <- names(data.out)
            names.out <-
                names.out[
                    unlist(lapply(names.out,function(x) nrow(unique(data.out[,c(cols.by,x)]))==Nid.out))
                ]
            data.out <- unique(data.out[,names.out,drop=F])
            
            ## report how many ids matched
            ## this will only work if length(cols.by)==1
            if(length(cols.by)==1){
                uids.out <- unique(data.out[,cols.by])
                nid.matched <- length(uids.out[uids.out%in%c(reduced.in[,cols.by,drop=T])])
                message(paste0(nid.matched,"/",length(uids.out)," IDs matched"))
            } 
            
            data.out.out <- merge(unique(data.out),reduced.in,by=cols.by,all.x=T)
        }
        
        if(!is.null(tab.id)){
            tab.id <- fun.merge(data.out=tab.id,data.in=data.input,cols.by=c(col.id,col.grp),debug=F)
        }

        if(!is.null(tab.occ)){
            tab.occ <- fun.merge(data.out=tab.occ,data.in=data.input,cols.by=c(col.id,col.grp,col.occ),debug=F)
        }
    }


    if(use.input&&reconstructRows){
        ## browser()
        inp.touse <- data.input[setdiff(data.input[,col.row],tab.row[,col.row]),]
        n.inp.touse <- names(inp.touse)
        inp.touse$nmout <- FALSE
        if(col.id%in%n.inp.touse) {
            ## browser()
            inp.touse <- merge(inp.touse,tab.id[,c(col.id,col.grp,setdiff(names(tab.id),n.inp.touse))],all.x=T)
        }
        if(col.occ%in%n.inp.touse) {
            inp.touse <- merge(inp.touse,tab.occ[,c(col.id,col.occ,col.grp,setdiff(names(tab.occ),n.inp.touse))],all.x=T)
        }
    ##    browser()
        tab.row <- rbindUnion(tab.row,inp.touse)
        tab.row <- tab.row[order(tab.row[,col.row]),]
    }

    stopifnot(max(table(col.row))==1)
    
    list.str <- list(col.row=col.row,
                     col.id=col.id,
                     col.occ=col.occ,
                     col.grp=col.grp)

    list(row=tab.row,id=tab.id,occ=tab.occ,list.str=list.str)
    
}



if(FALSE){
    dat1 <- NMscan6("e:/Project/NN7170_N8-GPsc/NN7170-4213/current/Nonmem/cohort4/181",col.row="ROWID")
    names(dat1)
    lapply(dat1,dim)
    lapply(dat1,head)


### looks right. Model only run on DRUG==1
    dat2 <- NMscan6("e:/Project/NN1406/4218/current/Nonmem/result_meeting/GIR_NNC0143-0406/GIR_NNC0143-0406_FINAL",col.occ="PROFID",col.grp="DRUG")
    lapply(dat2,dim)
    lapply(dat2,head)
    as.numeric(object.size(dat2))/1024^2
    dat2$id

    c1 <- NMimport("e:/Project/NN1406/4218/current/Nonmem/result_meeting/GIR_NNC0143-0406/GIR_NNC0143-0406_FINAL/TABLE_OUTPUT.txt")
    dim(c1)

#### concizu
    ## iov on bioavailability. It should be in the occ table
    dat3 <- NMscan6("e:/Project/NN7415_anti-TFPI/NN7415-4159/current/Nonmem/result_meeting/PK/PK_final",col.occ="OCC")
    lapply(dat3,dim)
    lapply(dat3,head)

    dat4 <- NMscan6("e:/Project/NN8640/NN8640-ph1PK/current/Nonmem/PK_popAll/runFinal",col.occ="OCC")

    ###### OCC is not in output tables and input data doesnt exist. So occ-level cannot be reconstructed.
    dat4 <- NMscan6("e:/Project/NN9924/Meta-analyses/ClinPharmModel/current/Nonmem/PK_final/005",col.occ="OCC",debug=F)

    lapply(dat4,dim)
    lapply(dat4,head)
    
    dat5 <- NMscan6("E:/Project/NN8640/NN8640-4042/current/Nonmem/IGFI/002",col.row = c("TRIAL", "ID","GRP","TIME","EVID"))
    

}


