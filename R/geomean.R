##' calculate geometric mean. 
##'
##' @param x vector to calculate the geometric mean of
##' @param z.rm removes zeros before calculation?
##' @param ci if TRUE, a data.frame including point estimate and confidence interval returned. If FALSE, a numeric representing the mean value returned.
##' @param dist.ci The distribution to use for the confidence interval. Default and only supported is "t".
##' @param p.ci probability covered by confidence interval. Default is 0.95
##' @param colnames If ci, this defines the column names of the resulting data frame. Default is c("est","ll","ul").
##' @family Calc
##' @importFrom stats sd qt
## Don't export. Use means instead.


geomean <- function(x,z.rm=FALSE,ci=FALSE,dist.ci="t",p.ci=.95,colnames=c("est","ll","ul")) {
    warning("geomean is deprecated. Use means(x,type=\"geometric\").")
  
    if(z.rm) x <- x[x!=0]
    lx <- log(x)
    
    est <- exp(mean(lx))
    if(!ci) return(est)

    if(!dist.ci=="t") stop("Only t-dist supported.")
    nobs <- length(x)
    w.ci <- qt(p=1-(1-p.ci)/2,df=nobs-1)*sd(lx)/nobs
    df <- data.frame(est=est,ll=est-w.ci,ul=est+w.ci)
    colnames(df) <- colnames
    df
}
