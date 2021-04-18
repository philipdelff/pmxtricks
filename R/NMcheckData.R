
## convert to data.table

## checks for NM compatibility.
### Report which can be used and which cannot.
### error if EVID, DV, AMT etc are not numeric to Nonmem

## run flagsAssign to summarize all findings

NMcheckData <- function(data,col.id="ID",col.time="TIME"){

    ### EVID must be in c(0,1,2,3,4)

    ## ID 
### Warning if the same ID is in non-consequtive rows

### within ID, time must be increasing
    data[,checkTimeInc:=c(TRUE,diff(get(col.time))>=0),by=col.id]

    data[]
    
}
