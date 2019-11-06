##' automatically find Nonmem tables and organize data
##'
##' @param file A nonmem control stream or output file from nonmem (.mod or
##'     .lst)
##' @param col.id The name of the subject ID variable, default is "ID".
##' @param col.row A column that is unique for each row. Such a column is needed
##'     for this function to work well.
##' @param col.grp If present, ID and OCC level info is grouped by col.grp. So
##'     should only be needed for cross-over.
##' @param col.occ The name of a non-mandatory occasion variable (say "OCC").
##' @param structure Either "full" or something else. If full, all variables that can be represented will be included at all levels. If not, only row-level data will be included in $row, only occasion-level data in $occ, etc.
##' @param use.input Merge with columns in input data? Using this, you don't
##'     have to worry about remembering including all relevant variables in the
##'     output tables.
##' @param reconstructRows Include rows from input data files that do not exist
##'     in output tables? A column called nmout will be TRUE when the row was
##'     found in output tables, and FALSE when not.
##' @param debug start by running browser()?
##'
##' @details This function makes it very easy to collect the data from a Nonmem
##'     run. Only, you have to make sure to include a row counter in your input
##'     data files and your output tables. It reorganises the data into four different levels
##' \itemize{
##'   \item run
##'   \item id
##'   \item occ
##'   \item row
##' }
##' @family DataWrangling
##' @import stats
##' @export



#### change log
## adding possibility to stack with discarded lines from input data.
#### end change log


### todo
## No longer sure this is an issue with the new data combination method: check if variables are consistent within ROW: ID (others?) This is fatal and will happen when using long ID's and non-matching format when writing tables from Nonmem.

## bug: skip input data if not found.

## exit if no tables are found

## use default values for col.grp and col.occ. Use if present.

## TODO: check overview.tables. Either they must be firstonly, or they must be full.length.

## TODO: col.row can only be used if found in both input and at least one output table.

## TODO: There are certain variables that can only be row specifc: WRES, CWRES, etc.

### end todo 



NMscanDataDT <- function(file,col.id="ID",col.row="ROW",col.grp=NULL,col.occ="OCC",structure="full",use.input=T,reconstructRows=F,debug=F){

    if(debug) browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    firstonly <- NULL
    has.row <- NULL
    type <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
###{ process arguments 
    file <- filePathSimple(file)
    if(!file.exists(file)) stop(paste0("Model file ",file," does not exist."),call. = F)
    dir <- dirname(file)

###}


###{ read all output tables and merge to max one firstonly and max one row
    tables <- NMscanTables(file,details=T,dt=T)
    data <- tables$data
    overview.tables <- tables$meta

#### TODO: check overview.tables. Either they must be firstonly, or they must be full.length.
### TODO: col.row can only be used if found in both input and at least one output table.
    
    
#### add has.grp, has.occ, has.id?
    fun.has.row <- function(names) do.call(c,lapply(names,function(name)col.row%in%colnames(data[[name]])))
    overview.tables[,has.row:=fun.has.row(name)]
######## here
    overview.tables[,maxLength:=nrow==max(nrow)]
    overview.tables[,full.length:=!firstonly&maxLength]
    NrowFull <- overview.tables[full.length==TRUE,unique(nrow)]

    ## browser()
    
### combine full tables into one
    tabs.full <- which(overview.tables$full.length)
    if(overview.tables[,sum(full.length)]==0) {
        stop("No full-length tables found. This is currently not supported (but should be, sorry).")
    }
    if(!overview.tables[,sum(has.row)]) {
        warning("col.row not found in any full.length tables. This is experimental. Input data cannotbe used.")
        use.input <- FALSE
    }
    tab.row <- NULL
    ##    if(sum(overview.tables$full.length&overview.tables$has.row)){
    if(any(overview.tables[,full.length&has.row])){
        ## take row column from the first table in which it appears.
        first.table.with.row <- data[[overview.tables[has.row==TRUE&full.length==TRUE,name[1]]]]
        tab.row <- data.table(col.row=first.table.with.row[,get(col.row)])
    } else {
        tab.row <- data.table(col.row=1:NrowFull)
    }
    
    setnames(tab.row,old="col.row",new=col.row)
    
    for(I in which(overview.tables[,full.length])){
        tab.row <- cbind(tab.row,data[[I]][,setdiff(names(data[[I]]),names(tab.row)),with=F])
    }


### combine firstonly tables into one
    tabs.firstonly <- which(overview.tables$firstonly)
    tab.firstonly <- NULL
    if(length(tabs.firstonly)){
        tab.firstonly <- data.table(col.id=data[[tabs.firstonly[1]]][,col.id,with=FALSE])
        ## setnames(all.row,old="col.id",new=col.id)
        for(I in tabs.firstonly){
            ## mergeCheck?
            tab.firstonly <- merge(all.firstonly,data[[I]][,c(col.id,setdiff(names(data[[I]]),names(all.firstonly)))],by=col.id)
        }
    }

    ## data2 <- data[-c(tabs.full,tabs.firstonly)]
    ## data <- c(data2,list(all.row),list(all.firstonly))


###### all row tables combined into one
###}

###{ split tables into row, id, and occ level
### for each table
    ## scan for covariates
    ## scan for occasion variables
    ## check if col.row is present. If so, look for row-level info



###{ handle input data
    if(use.input){
        
        data.input <- as.data.table(NMtransInput(file,debug=F))

        ## tab.row.1 <- copy(tab.row)
        ## tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T)
        tab.row <- merge(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T)
        
    }

    

    

##### TODO: There are certain variables that can only be row specifc: WRES, CWRES, etc.
    t1 <- Sys.time()
    t0 <- t1
    if(structure=="full"){

        ## tab.row
        if(is.null(tab.row)){
            all.row <- NULL
            tab.occ <- NULL
        } else {
            ## t2 <- Sys.time()
            ## cat("t2", t2-t0,"\n")
            ## t0 <- t2
            
            all.row <- tab.row
            if(!is.null(tab.firstonly)){
                all.row <- merge(tab.row,
                                 tab.firstonly[,c(col.id,setdiff(names(tab.firstonly),names(all.row))),with=FALSE],
                                 by=col.id)

                ## t3 <- Sys.time()
                ## cat("t3: ", t3-t0,"\n")
                ## t0 <- t3
                
            }
            ## tab.occ
            if(col.occ%in%colnames(all.row)){
                
                ## Sys.sleep(2)
                ## t1 <- Sys.time()
                ## tab.occ <- findCovs2(all.row,cols.id=c(col.id,col.occ),debug=F)
                ## t2 <- Sys.time()
                tab.occ <- findCovs(all.row,cols.id=c(col.id,col.occ),debug=F)
                ## t3 <- Sys.time()
                ## t3b <- Sys.time()
                ## tab.occ <- findCovs_df(all.row,cols.id=c(col.id,col.occ),debug=F)
                ## t4 <- Sys.time()
                
                ##  t4 <- Sys.time()
                ## cat("t4: ", t4-t0,"\n")
                ##  t0 <- t4
                
            } else {
                tab.occ <- NULL
            }
        }

        ## tab.id
        
        tab.id <- findCovs(all.row,cols.id=c(col.id))
        ## t5 <- Sys.time()
        ## cat("t5: ", t5-t0,"\n")
        ## t0 <- t5

        
        tab.run <- findCovs(all.row)
        ## t6 <- Sys.time()
        ## cat("t6: ", t6-t0,"\n")
        ## t0 <- t6
        
        
    } else {
        stop("only structure=full is implemented.")
    }

###}


###}

    if(use.input&&reconstructRows){
        stop("row reconstruction not implemented yet")
        
        ## browser()
        inp.touse <- data.input[setdiff(data.input[,col.row],tab.row[,col.row]),]
        n.inp.touse <- names(inp.touse)
        inp.touse$nmout <- FALSE
        if(col.id%in%n.inp.touse) {
            ## browser()
            inp.touse <- merge(inp.touse,tab.id[,c(col.id,col.grp,setdiff(names(tab.id),n.inp.touse))],all.x=T)
        }
        if(col.occ%in%n.inp.touse) {
            inp.touse <- merge(inp.touse,tab.occ[,c(col.id,col.occ,col.grp,setdiff(names(tab.occ),n.inp.touse))],all.x=T)
        }
        ##    browser()
        tab.row <- rbindUnion(tab.row,inp.touse)
        tab.row <- tab.row[order(tab.row[,col.row]),]
    }
###}

    stopifnot(max(table(col.row))==1)



    list.str <- list(
        col.id=col.id,
        col.row=col.row,
        col.occ=col.occ,
        col.grp=col.grp)

    list(run=tab.run,
         row=tab.row,
         id=tab.id,
         occ=tab.occ,
         list.str=list.str)

}
