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

### this function is used to replace .csv or whatever ending is used to .rds, .RData etc. file is path, ext is extension without ., e.g. "rds".
    transFileName <- function(file,ext){
        file.new <- sub("\\.[^\\.]+$",paste0(".",ext),file)
        file.new
    }

    
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
        file.RData <- transFileName(file,"RData")
        ## browser()
        assign(name.data,data.out)
        save(list=name.data,file=file.RData)
        written <- TRUE
    }
    if(write.rds){
        ## A dot and then something is needed in the name for us to be able to
        ## translate
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .rds. Choose a .csv file name.")

        file.rds <- transFileName(file,"rds")
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

