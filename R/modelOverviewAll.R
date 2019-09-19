##' run modelOverview on many runs. But only if it hasn't been done already.
##' @param path The directory in which to look for runs
##' @param pattern.dirs Regular expression. Only directories whose name match
##'     this will be taken into account.
##' @param force Include runs in the summary even if not found.
##' @param debug start by calling browser()?
##' @export

modelOverviewAll <- function(path,pattern.dirs="^[0-9].*",force=FALSE,debug=F,...){
    if(debug) browser()

    modeldirs <- list.files(path,pattern=pattern.dirs)
    ## todo Check that dirs found

### todo: print the ones that are omitted due to missing table files.
### find existing all outputfile
    ## allouts <- unlist(lapply(outputfile,function(filename)
    ##     list.files(file.path(path,modeldirs),pattern=paste(filename,"$",sep=""),recursive=T,full.names=T)
    ##     ))

    allouts <- list.files(file.path(path,modeldirs),pattern=paste("^output.txt$"),recursive=T,full.names=T)
    
    ##    filenames <- basename(allouts)

    ## reduce by removing the ones that contain a non-empty dir called
    ## model_diagnostics
    
    up2date <- sapply(dirname(allouts),function(dir){
        if(force) {
            return(FALSE)
        } else {
            dir.diag <- file.path(dir,"model_summary")
            return(dir.exists(dir.diag)&&length(list.files(dir.diag)))
        }
    })
    allouts <- allouts[!up2date]
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    
#### exclude models that are still running
    running <- sapply(allouts,function(outfile)length(list.files(dirname(outfile),pattern="^OUTPUT$"))>0)
    allouts <- allouts[!running]
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    
#### some problems lead to huge output.txt files. We dont want to search those.
    file.ok <- sapply(allouts,function(outfile){
        file.size(outfile)<2e5
    })
    if(any(!file.ok)) warning ("an output.txt was too large. Check this yourself.")
    allouts <- allouts[file.ok]
    
    if(!length(allouts)) {
        cat("Nothing to be done.\n")
        return(invisible())
    }
    ##    filenames <- basename(allouts)


    
    ## these are relative to path
    allpaths <- dirname(allouts)

    silent <- lapply(1:length(allpaths),function(N){
        print(allpaths[N])
        modelOverview(path.run=allpaths[N],...)
    }
    )
    return(invisible())
}

