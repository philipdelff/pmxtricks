
##' Check data for Nonmem compatibility
##' @param data The data to check
##' @import NMdata

## convert to data.table

## checks for NM compatibility.
### Report which can be used and which cannot.
### error if EVID, DV, AMT etc are not numeric to Nonmem

## run flagsAssign to summarize all findings? 


NMcheckData <- function(data,col.id="ID",col.time="TIME",col.flagn=NULL,col.row=NULL,debug=F){

    if(debug) browser()
    
    data <- copy(as.data.table(data))

### if possible, only look at col.flagn==0

    if(!is.null(col.flagn) && col.flagn%in%colnames(data)){
        if(is.numeric(data[,get(col.flagn)])){
            data <- data[get(col.flagn)==0]
        }
    }

    tmprow <- tmpcol(data,base="ROW",prefer.plain=TRUE)
    data[,(tmprow):=.I]

    NMasNumeric <- function(x,warn=F) {
        if(warn){
            as.numeric(as.character(x))
        } else {
            suppressWarnings(as.numeric(as.character(x)))
        }
    }
    
    ## if fun does not return TRUE, we have a finding.
    ## col is the actual column to be used for the condition,
    ## colname is the column name reported to user.
    listEvents <- function(col,name,fun,colname=col,dat=data,events=NULL,invert=FALSE,debug=F){
        if(debug) browser()

        if(invert){
            row <- dat[fun(get(col))==TRUE,get(tmprow)]
        } else {
            row <- dat[fun(get(col))!=TRUE,get(tmprow)]
        }

        if(length(row)==0) {
            res <- data.table(check=name,column=colname,row=NA)[0]
        } else {
            res <- data.table(check=name,column=colname,row=row)
        }
        rbind(events,res,fill=TRUE)
    }

    findings <- data.table(check="is NA",column="TIME",row=NA)[0]

    cols.num <- c("TIME","EVID","ID","CMT")

### Others that must be numeric?
    ## if MDV is found, add it here
    ## if MDV not found, record that without a row number
    cols.num.if.avail <- c("MDV",col.flagn)
    for(col in cols.num.if.avail){
        if(!is.null(col)&&col%in%colnames(data)){
            cols.num <- c(cols.num,col)
        } else {
            findings <- rbind(findings,
                              data.table(check="Column not found",column=col),
                              fill=TRUE)
        }
    }
### check for missing in cols.num
    newfinds <- rbindlist( lapply(cols.num,listEvents,name="is NA",fun=is.na,invert=TRUE,debug=F) )
    findings <- rbind(findings,
                      newfinds
                     ,fill=TRUE)

### check for  non-numeric in cols.num
    newfinds <- rbindlist( lapply(cols.num,listEvents,name="Not numeric",fun=NMisNumeric) )
    findings <- rbind(findings,
                      newfinds
                     ,fill=TRUE)


##### overwrite cols.num with NMasNumeric of cols.num
    data[,(cols.num):=lapply(.SD,NMasNumeric),.SDcols=cols.num]

### CMT must be a positive integer
    findings <- listEvents("CMT","CMT not a positive integer",fun=function(x)x>0&x%%1==0,events=findings)

### TIME must be positive
    findings <- listEvents("TIME","Negative time",fun=function(x)x>=0,events=findings)
    
### DV should be NA for dosing records
    findings <- listEvents("DV","DV not NA in dosing recs",fun=is.na,events=findings,dat=data[EVID%in%c(1,4)])
    
### Requirements to DV for EVID==2 and EVID==3?

### MDV should perfectly reflect is.na(DV)
    if("MDV"%in%colnames(data)){
        ## browser()
        data[,MDVDV:=MDV==as.numeric(is.na(DV))]
        findings <- listEvents("MDVDV","MDV does not match DV",colname="MDV",fun=function(x)x==TRUE,events=findings)
    }
    
### EVID must be in c(0,1,2,3,4)
    findings <- listEvents("EVID","EVID in 0:4",function(x) x%in%c(0:4),events=findings)
    
    ## ID 
### Warning if the same ID is in non-consequtive rows
    data[,ID.jump:=c(0,diff(get(tmprow))),by=col.id]
    findings <- listEvents("ID.jump",colname="ID",name="ID disjoint",fun=function(x) x<=1,events=findings)
    
### within ID, time must be increasing. Unless EVID%in% c(3,4) or events are jumped
    data[,newID:=get(col.id)!=shift(get(col.id),n=1)]
    data[1,newID:=TRUE]
    data[,reset:=EVID%in%c(3,4)]
    data[,newID:=cumsum(as.numeric(newID)+as.numeric(reset))]
    data[,checkTimeInc:=c(TRUE,diff(get(col.time))>=0),by=.(newID)]

    findings <- listEvents(col="checkTimeInc",name="Time increasing",function(x) !isTRUE(x),colname="TIME",events=findings)

### subjects without doses

### subjects without observations
    

### add ID to findings. Before or after 
    
    if(!is.null(col.id)){
        findings <- mergeCheck(findings,data[,c(tmprow,col.id),with=F],by.x="row",by.y=tmprow,all.x=T)
    }

### use the row identifier for reporting
    if(!is.null(col.row)){
        setcolnames(findings,"row",tmprow)
        findings <- mergeCheck(findings,data[,c(tmprow,col.row),with=F],by=tmprow,all.x=T)
        findings[,(tmprow):=NULL]
    }

    if(nrow(findings)==0) {
        message("No findings. Great!")
        invisible(findings)
    } else {
        print(findings[,.N,by=.(column,check)],row.names=FALSE)
        ##        cat("\n")
        return(findings)
    }
    
}
