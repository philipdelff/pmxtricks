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
## check if variables are consistent within ROW: ID (others?) This is fatal and will happen when using long ID's and non-matching format when writing tables from Nonmem.

## bug: skip input data if not found.

## exit if no tables are found

## use default values for col.grp and col.occ. Use if present.

### end todo 

NMscanData <- function(file,col.id="ID",col.row="ROW",col.grp=NULL,col.occ="OCC",structure="full",use.input=T,reconstructRows=F,debug=F){
    if(debug) browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

firstonly <- NULL
has.col.ROW <- NULL
type <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks



    
###{ process arguments 
    file <- filePathSimple(file)
    if(!file.exists(file)) stop(paste0("Model file ",file," does not exist."),call. = F)
    dir <- dirname(file)

###}


###{ read all output tables and merge to max one firstonly and max one row
    tables <- NMscanTables(file,details=T)
    data <- tables$data
    overview.tables <- tables$meta
    
#### add has.grp, has.occ, has.id?
    overview.tables <- cbind(overview.tables,setNames(as.data.frame(do.call(rbind,lapply(data,function(x)c(col.row%in%names(x))))),c("has.col.ROW")))
    overview.tables <- within(overview.tables,{maxLength=nrow==max(nrow)})

    overview.tables <- within(overview.tables,{full.length=!firstonly&maxLength&has.col.ROW})
    
    ## browser()
    
### combine full tables into one
    tabs.row <- which(overview.tables$full.length)
    if(!length(tabs.row)) stop("col.row not found in any full.length tables.")
    all.row <- NULL
    if(length(tabs.row)){
        all.row <- setNames(
            data.frame(col.row=data[[tabs.row[1]]][,col.row])
           ,col.row)
        for(I in tabs.row){
            all.row <- merge(all.row,data[[I]][,c(col.row,setdiff(names(data[[I]]),names(all.row)))])
        }
    }

### combine firstonly tables into one
    tabs.firstonly <- which(overview.tables$firstonly)
    all.firstonly <- NULL
    if(length(tabs.firstonly)){
        all.firstonly <- setNames(
            data.frame(col.id=data[[tabs.firstonly[1]]][,col.id])
           ,col.id)
        for(I in tabs.firstonly){
            ## mergeCheck?
            all.firstonly <- merge(all.firstonly,data[[I]][,c(col.id,setdiff(names(data[[I]]),names(all.firstonly)))])
        }
    }
    
    data2 <- data[-c(tabs.row,tabs.firstonly)]
    data <- c(data2,list(all.row),list(all.firstonly))
    
    
###### all row tables combined into one
###}

###{ split tables into row, id, and occ level
### for each table
    ## scan for covariates
    ## scan for occasion variables
    ## check if col.row is present. If so, look for row-level info

    coltypes <- lapply(1:length(data),function(I){
        dat <- data[[I]]
        cnames <- colnames(dat)
        cnames.left <- cnames

        tab.id <- NULL
        if(col.id%in%cnames.left){
            covs.maybe <- findCovs(dat[,cnames.left],cols.id=c(col.id,col.grp))
            vars <- setdiff(names(covs.maybe),c(col.row,col.id,col.occ,col.grp,"CWRES","RES","WRES","PRED","IPRE","DV"))
            if(length(vars)){
                tab.id <- data.frame(type="id",table=I,var=vars,stringsAsFactors=F)
                cnames.left <- setdiff(cnames.left,vars)
            }
        }

        tab.occ <- NULL
        if(all(c(col.id,col.occ)%in%cnames.left)){
            covs.maybe <- findCovs(dat[,cnames.left],cols.id=c(col.id,col.occ,col.grp))
            vars <- setdiff(names(covs.maybe),c(col.row,col.id,col.occ,col.grp,"CWRES","RES","WRES","PRED","IPRE","DV"))
            if(length(vars)){
                tab.occ <- data.frame(type="occ",table=I,var=vars,stringsAsFactors=F)
                cnames.left <- setdiff(cnames.left,vars)
            }
        }
        
        tab.row <- NULL
        if(length(cnames.left)){
            vars <- setdiff(cnames.left,c(col.row,col.id,col.grp))
            tab.row <- data.frame(type="row",table=I,var=vars,stringsAsFactors=F)
            cnames.left <- setdiff(cnames.left,vars)
        }
        
        rbind(tab.row,tab.id,tab.occ)
    }
    )
    
    coltypes <- do.call(rbind,coltypes)
###}

###{ reduce to the three data.frames we want    
###### now compare between tables and combine into one classsification of variables
    ## row-level and occ-level vars are only accepted from tables with most rows
    dims <- setNames(as.data.frame(do.call(rbind,lapply(data,dim))),c("nrow","ncol"))
    dims <- within(dims,{table=1:nrow(dims)})
    coltypes <- merge(coltypes,dims,all.x=T)
    coltypes <- within(coltypes,{maxLength=nrow==max(nrow)})
    coltypes <- subset(coltypes,!(type%in%c("row","occ")&!maxLength))

    ## first look at tables with max rows. Keep variables in only one of them. This is discarding redundant info.
    ## dismiss anything in tables with non-max nrows if it is present in a maxrow table    
    ## dismiss redundant info in non-max row tables.
    coltypes <- coltypes[with(coltypes,order(type,-xtfrm(maxLength),var)),]
    coltypes <- coltypes[!duplicated(coltypes[,c("type","var")]),]

### dismiss overlapping with priority: row, occ, id.
    coltypes <- within(coltypes,{type.tmp=factor(type,levels=c("row","occ","id"))})
    ## coltypes <- plyr::arrange(coltypes,type.tmp)
    coltypes <- coltypes[order(coltypes$type.tmp),]
    coltypes <- coltypes[!duplicated(coltypes[,c("var")]),]
    coltypes$type.tmp <- NULL

    
    
### create output data.frames
    ## coltypes
    
    ## row-level
    tab.row <- NULL
    if(any(coltypes$type=="row")){
        tab.row <- unique(do.call(rbind,
                                  lapply(data,function(x) {
                                      if(all(c(col.row,col.id,col.grp)%in%names(x))){
                                          x[,c(col.row,col.id,col.grp),drop=F]
                                      } else {
                                          NULL
                                      }
                                  })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="row"])
        for(I in tabs.to.merge){
            tab.row <-
                merge(tab.row,data[[I]][,c(col.row,subset(coltypes,table==I&type=="row")$var),drop=F],by=col.row)
        }
    }

    ## subject-level
    tab.id <- NULL
    if(any(coltypes$type=="id")){
        tab.id <- unique(do.call(rbind,
                                 lapply(data,function(x) {
                                     if(all(c(col.id,col.grp)%in%names(x))){
                                         x[,c(col.id,col.grp),drop=F]
                                     } else {
                                         NULL
                                     }
                                 })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="id"])

        for(I in tabs.to.merge){
            tab.id <-
                merge(tab.id,
                      unique(data[[I]][,c(col.id,col.grp,subset(coltypes,table==I&type=="id")$var),drop=F]),by=c(col.id,col.grp))
        }
    }
    
    ## occ-level
    tab.occ <- NULL
    if(any(coltypes$type=="occ")){
        tab.occ <- unique(do.call(rbind,
                                  lapply(data,function(x) {
                                      if(all(c(col.id,col.occ,col.grp)%in%names(x))){
                                          x[,c(col.id,col.occ,col.grp),drop=F]
                                      } else {
                                          NULL
                                      }
                                  })))
        ## now merge with all tables
        tabs.to.merge <- unique(coltypes$table[coltypes$type=="occ"])
        for(I in tabs.to.merge){
            tab.occ <-
                merge(tab.occ,
                      unique(data[[I]][,c(col.id,col.occ,col.grp,subset(coltypes,table==I&type=="occ")$var),drop=F]),by=c(col.id,col.occ,col.grp))
        }
    }
    
    if(structure=="full"){
        
        ## all merged
        tab.all <- data.frame(toremove=1)
        if(!is.null(tab.row)) tab.all <- merge(tab.all,tab.row)
        if(!is.null(tab.id)) tab.all <- merge(tab.all,tab.id)
        if(!is.null(tab.occ)) tab.all <- merge(tab.all,tab.occ)
        tab.all$toremove <- NULL
        tab.row <- tab.all

        ## put id level vars on occ
        if(!is.null(tab.occ)&&!is.null(tab.id)){
            tab.occ.id <- merge(tab.occ,tab.id,by=c(col.id,col.grp),all.x=T)
            tab.occ <- tab.occ.id
        }
        
    }
###}
###{ handle input data
    if(use.input){

        data.input <- NMtransInput(file,debug=F)
        
        cnames.in <- colnames(data.input)
        
        if(!is.null(tab.row)){
            ## browser()
            
            data.output <- tab.row
            cnames.out <- colnames(data.output)
            ## we need col.row
            stopifnot(col.row%in%cnames.in)
            stopifnot(col.row%in%cnames.out)
            
            vars.to.recover <- cnames.in[!cnames.in%in%cnames.out]
            dim.out.0 <- dim(data.output)
            data.output.out <- merge(data.output,data.input[,unique(c(col.row,vars.to.recover))],by=col.row,all.x=T)
            dim.out.1 <- dim(data.output.out)

            if(!all((dim.out.0+c(0,length(vars.to.recover)))==dim.out.1)){
                stop("Merge with input data resulted in unexpected number of columns. Please make sure that the $INPUT field in the .lst file matches the input dataset.")
            }
            data.output.out$nmout <- TRUE

            tab.row <- data.output.out[order(data.output.out[,col.row]),]
            
        }

        fun.merge <- function(data.in,data.out,cols.by,debug=F){
            if(debug) browser()
            cnames.in <- colnames(data.in)
            cnames.out <- colnames(data.out)
            ## if variables are in both in and output, output is preferred
            stopifnot(all(cols.by%in%colnames(data.in)))
            vars.common <- intersect(cnames.in,cnames.out)
            if(length(vars.common)){
                message("common variables in input and output. Only the ones from output are kept.")
            }
            vars.to.recover <- cnames.in[!cnames.in%in%cnames.out]
            cnames.in.no.id <- cnames.in[!cnames.in%in%cols.by]
            ## if these are present in input, they will be dropped
            cnames.drop <- c("DV","PRED","RES","WRES")
            
            cnames.to.use <- vars.to.recover[!vars.to.recover%in%cnames.drop]
            Nid <- nrow(unique(data.in[,cols.by,drop=FALSE]))
            names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data.in[,c(cols.by,x)]))==Nid))]
            ## browser()
            reduced.in <- unique(data.in[,c(cols.by,names.covs),drop=F])
            ## browser()

            ## and possibly reduce the output as well (e.g. row could easily be present)
            Nid.out <- nrow(unique(data.out[,cols.by,drop=FALSE]))
            names.out <- names(data.out)
            names.out <-
                names.out[
                    unlist(lapply(names.out,function(x) nrow(unique(data.out[,c(cols.by,x)]))==Nid.out))
                ]
            data.out <- unique(data.out[,names.out,drop=F])
            
            ## report how many ids matched
            ## this will only work if length(cols.by)==1
            if(length(cols.by)==1){
                uids.out <- unique(data.out[,cols.by])
                nid.matched <- length(uids.out[uids.out%in%c(reduced.in[,cols.by,drop=T])])
                message(paste0(nid.matched,"/",length(uids.out)," IDs matched"))
            } 
            
            data.out.out <- merge(unique(data.out),reduced.in,by=cols.by,all.x=T)
        }
        
        if(!is.null(tab.id)){
            tab.id <- fun.merge(data.out=tab.id,data.in=data.input,cols.by=c(col.id,col.grp),debug=F)
        }

        if(!is.null(tab.occ)){
            tab.occ <- fun.merge(data.out=tab.occ,data.in=data.input,cols.by=c(col.id,col.grp,col.occ),debug=F)
        }
    }

    tab.run <- NULL
    if(!is.null(tab.id)){
        tab.run <- findCovs(tab.id)
        if(structure!="full"){
            tab.id <- tab.id[,setdiff(names(tab.id),colnames(tab.run))]
        }
    }

    if(use.input&&reconstructRows){
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
