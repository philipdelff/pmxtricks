##### this function is experimental

## returns NULL if no args. should return a zero length char vector - should be fixed

## spaces cannot be handled - should be fixed

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
