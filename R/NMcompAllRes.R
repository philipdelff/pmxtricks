##' make sure all Nonmem results are saved in rds files
##'
##' @param dir The directory in which to look for nonmem results
##' @param ... passed to NMscanData
##' @param debug start by calling browser()?
##' @export


## look for lst files
## for each lst file, check if there is an rds that is newer than lst. If there is, exit.
## else, run NMscanData. Save results in an rds.

## "run01.mod" -> "run01_output.rds"

NMcompAllRes <- function(dir,...,debug=F){
    if(debug) browser()

    ## look for lst files
    files.lst <- list.files(dir,pattern="\\.lst$",full.names=T)

    ## for each lst file, check if there is an rds that is newer than lst. If there is, exit.
    lapply(files.lst,function(file.lst){
        run <- sub("\\.lst$","",file.lst)
        file.rds <- filePathSimple(paste0(run,"_output.rds"))
        if(!file.exists(file.rds) || (file.info(file.rds)$mtime<file.info(file.lst)$mtime) ){
            message(paste0("generating",file.rds))
            data <- NMscanData(file.lst,...)
            saveRDS(data,file=file.rds)
        }
    }
    )
    invisible(return())
}
