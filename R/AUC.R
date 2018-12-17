
trapez <- function(x, y, na.rm = FALSE){

	if((any(is.na(y)) | any(is.na(x))) & na.rm==F) 
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
