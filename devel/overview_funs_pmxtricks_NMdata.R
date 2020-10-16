## working towards release of pmxtricks v0.0.10 and NMdata v0.0.2. They should
## not export any common functions, but they should still be independent of each
## other. Only if/when NMdata gets more established and on CRAN, pmxtricks may
## start to depend on it.

library(devtools)

path.wcs <- "c:/Users/delff/working_copies"
load_all(file.path(path.wcs,"pmxtricks"))
load_all(file.path(path.wcs,"NMdata"))

help(package="pmxtricks")

## lsf.str("package:pmxtricks")



### overview of functions by family
dir.r <- file.path(path.wcs,"pmxtricks/R")
scripts.pmx <- list.files(dir.r)

fams <- lapply(scripts.pmx,function(s){

    code <- readLines(file.path(dir.r,s))
    code[grep("<- *function *\\(",code)]

})

names(fams) <- scripts.pmx
fams


##
funs.pmx <- sub("\\.R$","",scripts.pmx)
funs.pmx

### nmdata functions
dir.r <- file.path(path.wcs,"NMdata/R")
scripts.nmdata <- list.files(dir.r)

overviewScripts <- function(scripts,debug=FALSE){
    if(debug) browser()
    reps <- lapply(scripts,function(s){
        
        code <- readLines(file.path(s))

        rep <- data.table(
            path=s
           ,name=sub("\\.R$","",basename(s))
            ##name=code[grep("<- *function *\\(",code)]
           ,family=sub("^ *#+\\' *\\@family +","",code[grep("@family",code)])
           ,exported=any(grepl("@export",code))
           ,incl="internal"
        )
        rep[exported==TRUE,incl:="exported"]

        rep
        
    })
    rbindlist(reps)

}

dir.r <- file.path(path.wcs,"NMdata/R")
funs.nmd <- overviewScripts(file.path(dir.r,scripts.nmdata))
funs.nmd[,pkg:="NMdata"]

dir.r <- file.path(path.wcs,"pmxtricks/R")
funs.pmx <- overviewScripts(file.path(dir.r,scripts.pmx))
funs.pmx[,pkg:="pmxtricks"]

funs.all <- merge(funs.pmx[,.(name,incl)],funs.nmd[,.(name,incl)],all=T,by="name",suffixes=c(".pmx",".nmd"))
funs.all

### y=undecided:
## NMcode2R - so far only used by NMdata, but the user will need it to make use
## of i.e. genCovFun in pmxtricks.

funs.all[order(incl.nmd,incl.pmx)]

funs.all2 <- rbind(funs.pmx,funs.nmd)
funs.all2[,table(family,pkg,useNA="ifany")]
funs.all2[,.N,by=.(pkg,family,incl)]
funs.all2[is.na(family)]
