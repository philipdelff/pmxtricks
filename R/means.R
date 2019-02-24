##' calculate arithmetic or geometric mean and confidence intervals
##'
##' @param x vector to calculate the geometric mean of
##' @param type type of mean. Default is arithmetic, geometric is available as well. Only first letters needed, so say "geo" or even "g" is enough.
##' @param z.rm removes zeros before calculation? Default is FALSE. Can only be TRUE if type="geometric".
##' @param ci if TRUE, a data.frame including point estimate and confidence interval returned. If FALSE, a numeric representing the mean value returned.
##' @param dist.ci The distribution to use for the confidence interval. Default and only supported is "t". If type=geometric, this is applied after transformation to gaussian.
##' @param p.ci probability covered by confidence interval. Default is 0.95
##' @param colnames If ci, this defines the column names of the resulting data frame. Default is c("est","ll","ul").

### TODO
## 2019-02-22 philipdelff: Add support for median
## for CI:
## sort(x)[qbinom(c(.025,.975), length(x), 0.5)]

### TODO end

means <- function(x,type="arithmetic",z.rm=FALSE,ci=FALSE,dist.ci="t",p.ci=.95,colnames=c("est","ll","ul")) {

    type <- gsub("(^ +| +$)","",type)
    type <- tolower(type)

    if(type == substr("arithmetic",1,nchar(type))){
        type <- "arithmetic"
    } else if(type == substr("geometric",1,nchar(type))){
        type <- "geometric"
    } else {
        stop("type has to be the first letters of either arithmetic or geometric.")
    }

    
  if(type=="geometric"){
      if(z.rm) x <- x[x!=0]
        x <- log(x)
      } else {
        if(z.rm) stop("z.rm can only be TRUE when type==geometric")
      }
    est <- mean(x)
  
    if(!ci) {
      out <- est
      if(type=="geometric") out <- exp(est)
      return(out)
    }
      
    if(!dist.ci=="t") stop("Only t-dist supported.")
    nobs <- length(x)
    w.ci <- qt(p=1-(1-p.ci)/2,df=nobs-1)*sd(x)/nobs
    out <- c(est,est-w.ci,est+w.ci)

    out <- setNames(out,colnames)
    if(type=="geometric"){
      out <- exp(out)
    }
    out
}

