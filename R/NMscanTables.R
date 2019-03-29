##' read all output data tables in nonmem run
##' @param file the nonmem file to read (normally .mod or .lst)
##' @param details If TRUE, metadata is added to output. In this case, you get a list. I'd say, enable if doing programming.
##' @return A list of all the tables as data.frames. If details=TRUE, this is in one element, called data, and meta is another element. If not, only the element corresponding to data is returned.
NMscanTables <- function(file,details=F){

    dir <- dirname(file)
    extract.info <- function(x,NAME){
        r1 <- regexpr(paste0(NAME," *= *[^ ]*"),x)
        rm1 <- regmatches(x,r1)
        sub(paste0(NAME," *= *"),"",rm1)
    }
    
    lines.table <- NMgetSection(file,section="TABLE",keepName=F,keepComments=F,keepEmpty=F,asOne=F,simplify=F)


### TODO include firstonly
    tab.files <- lapply(lines.table,function(x) {
        tab <- data.frame(file=filePathSimple(dir,extract.info(x,"FILE"))
                         ,firstonly=any(grepl("FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY",x))
                         ,format=extract.info(x,"FORMAT")
                         ,stringsAsFactors=F)
        tab <- within(tab,{sep=ifelse(grepl(",",tab$format),",",".")})
        tab
    })

    meta <- do.call(rbind,tab.files)
    meta$nrow <- NA_real_
    meta$ncol <- NA_real_
    tables <- list()
    for(I in 1:nrow(meta)){
        tables[[I]] <- NMreadTab(meta[I,"file"],sep=meta[I,"sep"])
        dim.tmp <- dim(tables[[I]])
        meta[I,"nrow"] <- dim.tmp[1]
        meta[I,"ncol"] <- dim.tmp[2]
    }
    
    
    names(tables) <- meta$file
    
    
    if(details){
        out <- list(data=tables,meta=meta)
    } else {
        out <- tables
    }

    return(out)

}
