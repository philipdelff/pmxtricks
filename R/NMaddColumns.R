##' Merge columns onto all elements in a NM data object
##'
##' @param data A NM data object
##' @param df a data.frame to merge onto elements of data object.
##' 
##' @examples
##' df.races <- data.frame(RACE=c(1,3.1),
##'                        race1=c("White","Japanese"),
##'                        stringsAsFactors=F)
##' @export

NMaddColumns <- function(data,df){

    data.new <- lapply(data,function(d){
        if(!is.data.frame(d)) return(d)
        d1 <- mergeCheck(d,df.races,all.x=T)

        d1
    })

}
