##' Add meta data to an existing meta data object
##'
##' The simplest is using only arguments data, meta.data, variable,
##' and text (and unit if applicable). That will insert a line into
##' the metadata describing the variable. The other two methods are
##' for when a variable is a numeric code referring to factor levels
##' (i.e. GRP or POP). If the character variable describing the levels
##' is already present in data, use variable and var.char. If the
##' levels are described in a data.frame, pass this data.frame as
##' values. 
##'
##' @param data The dataset being described (i.e. a data.frame).
##' @param meta.data The existing meta.data object (see
##'     \code{metaInit}).
##' @param variable The name of the variable in the dataset to
##'     describe. Must supply either this or \code{header}.
##' @param text Description of \code{variable}. If using
##'     \code{header}, \code{text} is not used.
##' @param header Append lines to the header of the meta data. The
##'     lines will be added in the order they are given.
##' @param var.char Match the numerical values of \code{variable}
##'     with character values in this column. Say, variable is ETHN,
##'     maybe var.char is ethnicity.
##' @param var.unit Take unit from a column in data. The column
##'     must contain only one unique variable. NA values in the column
##'     are disregarded.
##' @param values A data.frame with character values to merged with
##'     variable. values must contain a numeric column with the same
##'     name as variable. You cannot use both var.char and values.
##' @param unit The unit of the variable. Will be printet in parenthesis when listing variables.
##' @param debug Start by calling browser().
##' @return Updated meta data object
##' @export
##' @family DataGen: Meta


#### TODO
## If a numeric covariate is given including a var.char, and they are
## missing in some rows (could be missing for say doses), an entry
## like NA: NA will appear meta data print. I guess this should be
## cleaned.

#### End TODO

#### Bugs

### End bugs



metaAdd <- function(data,meta.data,variable=NULL,text=NULL,header=NULL,var.char=NULL,var.unit=NULL,values=NULL,unit=NA,debug=F){##### metaAdd provides three different methods for adding meta data.

    if(debug)browser()
    data <- as.data.frame(data)
    datacols <- colnames(data)

    ## either we are adding a variable or a header line. Not both.
    if((is.null(variable)+is.null(header))!=1) {stop("You must add either a variable or a header line.")}
    
    if(!is.null(var.char)&&!is.null(values)) {stop("Either var.char or values can be supplied. Not both.")}
    
### header part.    
    if(!is.null(header)){
        meta.data$meta <- c(meta.data$meta,as.list(header))
        ## we're done
        return(meta.data)
    }

### We now know that a variable was given.
    ## varible must exist in dataset
    if(!variable%in%datacols) stop(paste("No varible named",variable,"exists in dataset. variable must refer to an existing column in data."))
    ## Then text is a must.
    if(is.null(text)){stop("You must supply a description of the variable in text.")}
    ## If var.char is given, it must be in data
    if(!is.null(var.char)&&!var.char%in%datacols){stop("var.char does not point to a column that exist in data.")}
    ## If var.unit is given, it must be in data
    if(!is.null(var.unit)&&!var.unit%in%datacols){stop("var.unit does not point to a column that exist in data.")}

    
### If the variable is already present in meta.data$variables, it must be removed first so this entry will be unique.
    var.arg <- variable
    meta.data$variables <- subset(meta.data$variables,variable!=var.arg)
    
        
### values supplied in data.frame
    if(!is.null(values)){
        if(!is.data.frame(values)){stop("values must be a data.frame")}
        if(ncol(values)!=2) stop("values must have exactly two columns")
        if(!variable%in%colnames(values))stop("values must contain a column named like the variable you are documenting.")
        if(!is.numeric(values[,variable])){stop("The values column named as the contents of variable must be numeric")}
        ##   maybe this should rather be done like
        ##   sum(unlist(lapply(value,is.numeric)))!=1 ? I mean, could other
        ##   classes be ok and the values still numeric?
        classes <- sapply(values,class)
        col.num <- which(classes%in%c("numeric","integer"))
        col.char <- which(classes%in%c("character","factor"))
        if(length(col.num)!=1||length(col.char)!=1){stop("values must contain exactly one numeric column and one column of class character or factor.")}
        
#### have to check that all values are matched
        if(any(!unique(data[,variable])%in%values[,variable])){stop(paste("Not all values of column",variable,"is matched by values supplied in values."))}
        
        var.char <- colnames(values)[col.char]
        
        data <- merge(data,values,all.x=T)
    }

    if(!is.null(var.char)){        
        temp=unique(data[,c(variable,var.char)])
        temp=temp[order(temp[,variable]),]
        ## need check that NA match NA and nothing else
        ## temp <- as.data.frame(lapply(temp, function(x){
        ##     x[gsub(" ","",x)==""] <- NA
        ##     x
        ## }))
#### this checks if NA paired with non-NA. The check is relevant but not here. Maybe a warning could be given. 
        ## if(any(rowSums(is.na(temp))==1)){
        ##     stop("variable and character variable combinations pair NA with non-NA. This means that you either have NA in variable while not in var.char or the other way around. That is not allowed.")
        ##     }
        values.out=paste(  paste( temp[,1],temp[,2],sep=": "),collapse="; ")
        
    } else {
        values.out <- NA
    }

###### Handling of units.
### Check that only one of unit and var.unit are supplied
    if((!is.na(unit)+!is.null(var.unit))==2){stop("You cannot use both unit and var.unit.")}
### if var.unit is supplied, we take unit from there. If NA's should be in present, they are removed without warning. This could be the case for say dose.unit if that was assigned to doses dataset which was later rbind'ed with pk data.
    if(!is.null(var.unit)){
        units.in.data <- unique(data[!is.na(data[,var.unit]),var.unit])
        if(length(units.in.data)>1) {
            stop(paste0("Exiting. More than one unique values is found in unit.var:\n",paste(units.in.data,collapse="\n")))
        }
        unit <- units.in.data
    }
    
    
    meta.data$variables <- rbind(meta.data$variables,
                                 data.frame(variable=variable,
                                            unit=unit,
                                            description=paste(text,sep=" - "),
                                            values=values.out)
                                 )
    
### order variable table according to data
    ##    meta.data1[var]
    
    
    ## Sort meta
    meta.data$variables <- meta.data$variables[order(match(meta.data$variables$variable,datacols)),]
    rownames(meta.data$variables) <- 1:nrow(meta.data$variables)
    
    return(meta.data)
    
}
