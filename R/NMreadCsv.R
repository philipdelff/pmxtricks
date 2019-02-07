## read csv data that is written to be read from nonmem
NMreadCsv <- function(file,na.strings=".",header=TRUE,stringsAsFactors=FALSE,...){
    read.csv(file=file,na.strings=na.strings,header=header,stringsAsFactors=stringsAsFactors,...)
}
