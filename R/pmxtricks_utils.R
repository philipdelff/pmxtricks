##' get a file or dir from a the pmxtricks package
##' @param ... parameters to pass to system.file
##' @details
##' a light wrapper around system.file
##' @family FileSystem
##' @export
pmxtricks_filepath <- function(...) {
  system.file(..., package = "pmxtricks")
}
