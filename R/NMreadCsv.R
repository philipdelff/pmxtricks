## read csv data that is written to be read from nonmem
read.csv.nm <- function(file,...){
    read.csv(file=file,na.strings=".",stringsAsFactors=FALSE,...)
}
