##' Get and source an R script.
##' 
##' Get file from remote library and inport it into project if it does not exist. Then source it. 
##'
##' @param file File name of wanted file
##' @param dir.central Folder path of wanted file
##' @param dir.local Where to put the file if imported, and where to look for
##'     already imported files. Default is getwd().
##' @param overwrite Owerwrite previously imported file?
##' @param source.directly Enables direct sourcing of the central file copy. This
##'     bypasses the whole concept of the function but it is useful when
##'     developing while using a function. Especially if your debugger in the
##'     editor is linking to a file that you will edit while debugging. It gives
##'     a warning because it is not recommended in final code.
##' @param silent Disables printning. Mainly used in testing.
##' @family FileSystem
##' @return None. Sources the specified file into the global environment.
##' @export

getSource <- function(file,dir.central=NULL,dir.local,overwrite=FALSE,source.directly=FALSE,silent=F){
    
### Getting the paths right
    if(is.null(dir.central)){
        dir.central <- dirname(file)
        file <- basename(file)
    }
    if(missing(dir.local)) dir.local <- getwd()
    org = file.path(dir.central,file)


    if(source.directly)
    {
        source(org)
        warning("Central file was sourced directly. Only allowed for debugging. Switch off and run once with overwrite=TRUE if you want to update. Seriously, use this option for debugging only.")
        return(invisible())
    }

    dir.local <- filePathSimple(dir.local)
    dir.central <- filePathSimple(dir.central)
    
    ## It should be checked that destination folder exists before doing anything.
    if(!dir.exists(dir.local)) {stop("Destination directory (dir.local) must exist.")}

    ## Check that source directory is different from destination directory
    if(filePathSimple(dir.central)==filePathSimple(dir.local)){
        stop("source and destination directories are identical. Makes no sense.")
    }
    
    dest = file.path(dir.local,file)
  
  ## do necessary files and dirs exist?
  if (!dir.exists(dir.central) || !file.exists(org)){
    if(file.exists(file.path(dir.local,file))){
      #File exists locally, but not external. Load local
      source(file.path(dest),echo=F)
      warning("File not found at dir.central. Local version has been imported.")
      return(invisible())
    } else {
      if (silent == F){message("No local version have been found.\n")}
      stopifnot(file.exists(dir.central))
      stopifnot(file.exists(org))
    }
  }
  ## Checking whether there is a local version and if it missing, it is copied
  if(!file.exists(dest) || overwrite){
    ## Copying the latest version of the file
    if (silent == F){message("Copying ",file,"\n")}
    file.copy(from=org,
              to=dest,overwrite=TRUE)
  }
  ## Sourcing the file
  source(dest,echo=F)
}

