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

NMaddColumns <- function(data,df,by,debug=T){
    if(debug) browser()
    data.new <- lapply(data,function(d){
        if(!is.data.frame(d)||!all(by%in%names(d))) return(d)
        d2 <- try( mergeCheck(d,df,by=by,all.x=T))
        if("try-error"%in%class(d2)) d2 <- d
        d2
    })

}
