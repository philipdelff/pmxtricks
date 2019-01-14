
######## now read output data tables
NMscanTables <- function(file,details=F){
    dir <- dirname(file)
    extract.info <- function(x,NAME){
        r1 <- regexpr(paste0(NAME," *= *[^ ]*"),x)
        rm1 <- regmatches(x,r1)
        sub(paste0(NAME," *= *"),"",rm1)
    }
    
    lines.table <- NMgetSection(file,name="TABLE",keepName=F,keepComments=F,keepEmpty=F,asOne=F,simplify=F)


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
