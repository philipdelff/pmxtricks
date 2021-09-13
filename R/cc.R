##' Create character vectors without quotation marks
##' 
##' When creating character vectors with several elements, it becomes a lot of quotes to type. cc provides a simple way to skip the quotes.
##' @param ... The unquoted names that will become character values in the returned vector.

##' @export

##' @examples
##' cc(a,b,`a b`)
##' ## be careful with spaces
##' cc( d)
##' cc(` d`)
##' cc()

cc <- function(...){

    list.names <- as.list(match.call())[-1]
    
    out <- do.call(c,
            ## lapply(as.list(match.call())[-1], function(x)   as.character(parse(text=x)))
            lapply(list.names, as.character)
            )

    if(is.null(out)) out <- character()
    out
}
