##' trapezoidal AUC on linear scale
##' @description This is a numerical integration of y with respect to x by the trapezoidal method on linear scale. 
##' @param x The vector to integrate y with respect to (typically TIME to get AUC).
##' @param y The variable to integrate.
##' @param na.rm Remove indexes in x and y wherever x or y are NA.
##' @family Calc
##' @export 
trapez <- function(x, y, na.rm = FALSE){

	if((any(is.na(y)) || any(is.na(x))) && !na.rm) 
	{
            ## warning("y or x contains NA with na.rm=F - returning NA\n")
		return(NA)	
	}
	
    ## Remove any missing values and issue warning if removing values
	if(na.rm) {
            ## Locate missing values in both vectors
		miss <- as.logical(is.na(x) + is.na(y))
		if(any(miss)) {
			x <- x[!miss]
			y <- y[!miss]
                        ## warning(paste(sum(miss), "missing values removed\n"))
			if(length(x) < 2) {
				warning("No valid observations remaining after NA removal")
				return(NA)	
			}
		}
	}
    
    
    dx <- diff(x)
    yfirst <- y[-length(y)]
    ylast <- y[-1]

    retVal <- sum(dx*(yfirst+ylast)) / 2
    
}
