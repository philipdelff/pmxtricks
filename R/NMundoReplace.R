##' simply overwrite input.txt with contents of backup_input.txt
NMundoReplace <- function(path){

    path <- file.path.simple(path)
    file <- path
    if(dir.exists(path)) {
        file <- file.path(path,"input.txt")
        file.backup <- file.path(path,"backup_input.txt")
    }
    stopifnot(file.exists(file))
    stopifnot(file.exists(file.backup))

### not to lose anything, a backup of the input file to be overwritten is kept in trash.
    file.trash <- file.path(path,"trashed_input.txt")
    
    file.copy(file,file.trash,overwrite=T)
    file.copy(file.backup,file,overwrite=T)


}

