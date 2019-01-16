## look for lst files
## for each lst file, check if there is an rds that is newer than lst. If there is, exit.
## else, run NMscanData. Save results in an rds.

## "run01.mod" -> "run01_output.rds"

NMcompAllRes <- function(dir,...){

    ## look for lst files
    files.lst <- list.files(dir,pattern="\\.lst$")

    ## for each lst file, check if there is an rds that is newer than lst. If there is, exit.
    lapply(files.lst,function(file.lst){
        run <- sub("\\.lst$","",lst)
        rds <- paste0(run,"_output.rds")
        file.rds <- file.path(dir,rds)
        if(!(file.exists(file.out)&&file.info(file.out)$mtime>file.info(file.lst)$mtime)){
            messager(paste0("generating",file.rds))
            data <- NMscanData(file.lst,...)
            saveRDS(data,file=file.path(dir,file.rds))
        }
    }
    )
    invisible(return())
}
