## factor with automated levels by sorting a corresponding variable (which can
## be the variable itself).
factor2 <- function(x,by=x,debug=F){
    if(debug) browser()

    library(data.table)

    DT1 <- data.table(x=x,by=by)
    DT1[,rank:=as.numeric(as.factor(frank(by,ties.method="min")))]

    DT2 <- unique(DT1)

    factor(x,levels=DT2[order(rank),x])
}
