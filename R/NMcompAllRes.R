##' make sure all Nonmem results are saved in rds files
##'
##' @param dir The directory in which to look for nonmem results
##' @param ... passed to NMcompRes which will pass them to NMscanData
##' @param debug start by calling browser()?
##' @family DataWrangling
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
    # lapply(files.lst,function(file.lst){
    #     run <- sub("\\.lst$","",file.lst)
    #     file.rds <- filePathSimple(paste0(run,"_output.rds"))
    #     if(!file.exists(file.rds) || (file.info(file.rds)$mtime<file.info(file.lst)$mtime) ){
    #         message(paste0("generating",file.rds))
    #         data <- NMscanData(file.lst,...)
    #         saveRDS(data,file=file.rds)
    #     }
    # }
    # )
    lapply(files.lst,function(x)try(NMcompRes(x,return=FALSE,...)))
    
    invisible(return())
}

##' Read and store nonmem results in rds
##'
##' Tries to find out if the output rds should be updated and does so if necessary.
##' 
##' @param file a nonmem control stream or output (normally, .mod or .lst).
##' @param return If TRUE, resulting data structure is invisibly returned. You may want to disable this if the rds is the goal itself. Disabling can save reading the rds if there is nothing to do.
##' @param ... passed to NMcompRes which will pass them to NMscanData
##' @export


NMcompRes <- function(file,return=TRUE,...){
   
  run <- sub("\\.lst$","",file)
  run <- sub("\\.mod$","",run)
  file.lst <- paste0(run,".lst")
  file.rds <- filePathSimple(paste0(run,"_output.rds"))
  wrote <- FALSE
  if(!file.exists(file.rds) || (file.info(file.rds)$mtime<file.info(file.lst)$mtime) ){
    message(paste0("generating ",file.rds))
    data <- NMscanData(file.lst,...)
    saveRDS(data,file=file.rds)
    wrote <- TRUE
  }
  
  if(return) {
    if(!wrote){
      data <- readRDS(file.rds)
    }
    return(invisible(data))
  }
  
}


