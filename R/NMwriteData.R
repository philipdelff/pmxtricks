### Todo

## The printed message should not contain lowercase names

## print out all dropped variables. Not warning. Warning if standard variable?

## Check that file ends in either csv or txt

NMwriteData <- function(data,file,drop=NULL,drop.lowercase=FALSE,write.csv=TRUE,write.RData=F,write.rds=write.csv,force.row=FALSE,debug=FALSE){
    if(debug) browser()
    stopifnot(is.data.frame(data))
    data.out <- data
    if(!is.null(drop)) data.out <- data.out[,-which(names(data.out)%in%drop),drop=FALSE]

    if(drop.lowercase) data.out <- data.out[,which(toupper(names(data.out))==names(data.out))]

   
    ## I guess we should always quote. Strings could contain commas.
    ## quote <- TRUE
    ## no, we must not quote. ID is often a character. If quoted, nonmem will not be able to read. So avoid commas in strings. Maybe look for commas and report error if found?
    quote <- FALSE

    ## only report numerics to user. But this is not good enough. Only report
    ## until first character. Moreover, it's not this easy. Variables may be
    ## character but still be interpretable as numeric. Often ID is like this.
    
    ## col.is.num <- sapply(data.out,is.numeric)
    ## stopifnot(any(col.is.num))
    
    cat("Nonmem data file:",file,"\n")
    cat("For NonMem:\n")
    cat("$INPUT",paste(colnames(data.out),collapse=" "),"\n")
    ##    cat("$DATA", sub(pattern="^E:/",replacement="/project/",file,ignore.case=TRUE))
    cat("$DATA", file)
    ## if(!quote) cat(" IGN=@")
    cat(" IGN=@")
    if("FLAG"%in%colnames(data.out)) cat(" IGNORE=(FLAG.NE.0)")
    cat("\n")

    written <- FALSE
    if(write.csv){
        opt.orig <- options(scipen=15)
        write.csv(data.out,na=".",quote=quote,row.names=FALSE,file=file)
        options(opt.orig)
        written <- TRUE
    }
    if(write.RData){
        name.data <- deparse(substitute(data))
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .RData. Choose a .csv file name.")
        file.RData <- sub("\\..+$",".RData",file)
        ## browser()
        assign(name.data,data.out)
        save(list=name.data,file=file.RData)
        written <- TRUE
    }
    if(write.rds){
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .rds. Choose a .csv file name.")
        file.rds <- sub("\\..+$",".rds",file)
        ## browser()
        saveRDS(data.out,file=file.rds)
        written <- TRUE
    }
    if(written){
        cat("Data file(s) written.\n")
    } else {
        cat("Data returned but not written to file(s).\n")
    }
    invisible(data.out)
}

