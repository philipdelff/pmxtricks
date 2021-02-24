## paste something before file name extension. If it's a number, we can pad some zeros if wanted.

fnAppend <- function(fn,x,pad0=0){
    
    if(!is.numeric(x)&&!is.character(x)) stop("x must be numeric or character.")

    fnroot <- sub("^(.+)\\..+$","\\1",fn)
    fnext <- sub(".*\\.([^\\.]+)$","\\1",fn)
    
    if(is.numeric(x)){
        string <- sprintf(fmt=paste("%0",pad0,"d",sep=""),x)
    } else {
        string <- x
    }

    paste(fnroot,"_",string,".",fnext,sep="")

}
