## returns NULL if no args. should return a zero length char vector
cc <- function(...){
    
do.call(c,
    lapply(as.list(match.call())[-1], function(x)   as.character(parse(text=x)))
    )
}
