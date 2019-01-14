## read csv data that is written to be read from nonmem
NMreadCsv <- function(file,...){
    read.csv(file=file,na.strings=".",stringsAsFactors=FALSE,header=T,...)
}
