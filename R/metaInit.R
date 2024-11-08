##' Initialise a metadata object based
##'
##' Create a metadata object based on a dataset and some initial
##' information. A metadata object is a list containing information
##' about variables in a dataset plus some more information such as
##' what dataset is being described, what study it is part of etc.
##'
##' @param data The dataset being described (i.e. a data.frame).
##' @param stamp The path to the script that is creating the data.
##' @param analysis The name of the analysis (character). This could be a
##'     trial name or any another character string.
##' @param header Append lines to the header of the meta data.
##' @param use.standard.cols The name is slightly misleading. If FALSE,
##'     only the header will be created. That means that unit arguments
##'     will have no effect.
##' @param time.unit The unit of time columns in data. Default is "h"
##'     for hours.
##' @param dv.unit If supplied, the unit for DV will be taken from
##'     here. a column called dv.unit will be ignored.
##' @param amt.unit If supplied, the unit for AMT will be taken from
##'     here. a column called amt.unit will be ignored.
##' @param ndos.unit If supplied, the unit for NDOS will be taken from
##'     here. 
##' @param debug Start by calling browser()?
##' @return Created meta data object
##' @importFrom utils read.csv
##' @family DataGen
##' @export



#### TODO

## Like dv.unit is only considered for !is.na(DV), the same should be done for
## amt.unit.

## Take a function as argument that translates from numeric to character variable. Could be tolower, function(x)sub("C$","",x), or a table lookup.

## automatically add: GRP MALE RACE ETHN?  These should be paired with lowercase.

## Automatically add NDOS? Nominal dose...

## Look for time.unit in data if not an argument

## ID as a character variable must be supported. Maybe check that it
## translates into a numeric?

##### END TODO

####### bugs

###### end bugs


metaInit <- function(data,stamp, analysis=NULL,header=NULL,use.standard.cols=T,time.unit="h",dv.unit=NULL,amt.unit=NULL,ndos.unit=NULL){

    
#### check arguments
    if(!is.null(analysis)) stopifnot(is.character(analysis))


    ## type variable descrip unit/possible vals
    meta <- list(DataCreateScript=stamp,
                 Analysis=analysis)
    meta.data <- list(meta=meta)

    if(!is.null(header)) meta.data$meta <- metaAdd(data=data,meta.data=meta.data,header=header)$meta

    if(!use.standard.cols) return(list(meta=meta))

    datacols <- colnames(data)

    if (is.null(dv.unit)){
        if("dv.unit"%in%datacols){
            if("DV"%in%datacols){
                dv.units <- unique(data$dv.unit[!is.na(data[,"DV"])])
            } else {
                dv.units <- unique(data$dv.unit)
            }
            if(length(dv.units)!=1){
                warning("Number of unique values in dv.unit where !is.na(DV) is not one. Does the dataset contain different observation variables?")
            }
            if(any(is.na(dv.units)|dv.units=="")){
                stop("Missing values in dv.unit where !is.na(DV). Fix the variable dv.unit in data, or remove the column and use the argument dv.unit instead.")
            }
            dv.unit=paste(dv.units,collapse=", ")
        } else {
            if("DV"%in%datacols){
                warning("No unit for DV found (no argument dv.unit, and no column dv.unit in data).")
            }
            dv.unit <- NA
        }
    }

    if (is.null(amt.unit)){
        if("amt.unit"%in%datacols){
            amt.unit=paste(unique(data$amt.unit[!is.na(data$amt.unit)]),collapse=", ")
        } else {
            if("AMT"%in%datacols){
                warning("No unit for AMT found (no argument amt.unit, and no column amt.unit in data).")
            }
            amt.unit <- NA
        }
    }

    if (is.null(ndos.unit)){
        if("ndos.unit"%in%datacols){
            ndos.unit=paste(unique(data$ndos.unit[!is.na(data$ndos.unit)]),collapse=", ")
        } else {
            if("NDOS"%in%datacols){
                warning("No unit for NDOS found (no argument ndos.unit, and no column ndos.unit in data).")
            }
            ndos.unit <- NA
        }
    }


    vars.default <- read.csv(stringsAsFactors=F,text=
                                                    "variable,description
AGE,Age
AMT,Actual dose amount
BMI,Body mass index
BW,Body weight
BLQ,Below LLOQ indicator
CMT,Compartment identifier
DLVL,Dose level
DV,Concentration
EVID,Event identifier
ID,Subject ID
MD,Dosing regimen
MDV,Missing dependent variable specifier
NDOS,Nominal dose
NOMTIME,Nominal time
ROW,Row number in NONMEM data file
TIME,Actual time
VISIT,Visit ID")

    bw.unit <- "kg"
    bmi.unit <- "kg/m2"
    age.unit <- "years"
    dlvl.unit <- ifelse("dlvl.unit"%in%datacols,unique(data$dlvl.unit),NA)

    vars.default <- merge(vars.default,
                          data.frame(variable=c("DLVL","DV","TIME","NOMTIME","NDOS","AMT","AGE","BW","BMI","BLQ","MD"),
                                     unit=c(dlvl.unit,dv.unit,time.unit,time.unit,ndos.unit,amt.unit,age.unit,bw.unit,bmi.unit,NA,NA),
                                     values=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"0: Not below LLOQ; 1: Below LLOQ","0: Single dose; 1: Multiple dose"),
                                     stringsAsFactors=F),
                          all.x=T)
    variables <- subset(vars.default,variable%in%datacols)

    meta.data <- list(meta=meta,
                      variables=variables)

    if(all(c("DLVL","dlvl")%in%datacols)){
        ##        message("DLVL and dlvl found in data.")
        if(!"dlvl.unit"%in%datacols) warning("No unit found for DLVL. Add column dlvl.unit to fix this.")
        meta.data <- metaAdd(data=data,meta.data=meta.data,variable="DLVL",var.char="dlvl",var.unit=if("dlvl.unit"%in%datacols) "dlvl.unit" else NULL, text="Dose level")
        message("DLVL added to meta.data (based on dlvl).")
    }
    if(all(c("FLAG","flag")%in%datacols)){
        ##        message("FLAG and flag found in data.")
        meta.data <- metaAdd(data=data,meta.data=meta.data,variable="FLAG",var.char="flag",text="Exclusion flag")
        message("FLAG added to meta.data (based on flag).")
    }
    if(all(c("POP","pop")%in%datacols)){
        ##        message("POP and pop found in data.")
        meta.data <- metaAdd(data=data,meta.data=meta.data,variable="POP",var.char="pop",text="Population group")
        message("POP added to meta.data (based on pop).")
    }
    if(all(c("ROUTE","route")%in%datacols)){
        ##        message("ROUTE and route found in data.")
        meta.data <- metaAdd(data=data,meta.data=meta.data,variable="ROUTE",var.char="route",text="Administration route")
        message("ROUTE added to meta.data (based on route).")
    }

    if(nrow(meta.data$variables)>0){
        message(paste0("The following variables were found in data and automatically documented in meta.data:\n",paste(meta.data$variables$variable,collapse="\n"),"\n"))
    }
    ## Sort meta
    if(nrow(meta.data$variables)){
        meta.data$variables <- meta.data$variables[order(match(meta.data$variables$variable,datacols)),]
        rownames(meta.data$variables) <- 1:nrow(meta.data$variables)
    }

    return(meta.data)

}
