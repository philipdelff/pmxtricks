dummyDF <- function(fun.format){

    df <- data.table(a=1,b=4,c="b")

    
    if(!missing(fun.format)){
        asfun <- fun.format
    } else if(!is.null(getOption("NMdata.asfun"))) {
        asfun <- getOption("NMdata.asfun")
    }
    df <- asfun(df)

    df
}
