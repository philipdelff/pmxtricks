## paste something before file name extension. If it's a number, we can pad some zeros if wanted. An underscore will be separating fn and x.

fnAppend <- function(fn,x,pad0=0,sep="_"){
    
    if(!is.numeric(x)&&!is.character(x)) stop("x must be numeric or character.")

    fnroot <- sub("^(.+)\\..+$","\\1",fn)
    fnext <- sub(".*\\.([^\\.]+)$","\\1",fn)
    
    if(is.numeric(x)){
        string <- sprintf(fmt=paste("%0",pad0,"d",sep=""),x)
    } else {
        string <- x
    }

    paste0(fnroot,sep,string,".",fnext)

}
