##' Export plots created with ggplot (and more) to files (png or pdf) - or show them on screen.
##'
##' @param plot A plot object or a list of plots. Normally generated with ggplot
##'     or qplot. But it can also be from grid.arrange or arrangeGrob with class
##'     gtable. That is experimental though. Not sure exactly what classes are
##'     supported.
##' @param file A file to export to. Must end in .png or .pdf. If plot is a
##'     list, see onefile. If missing, plot is shown on screen.
##' @param stamp This should normally be the path to your script. Requires
##'     ggplot >=2.2.1.
##' @param canvas Either a list of height and width or a shortname of predefined
##'     canvas size. See ?canvasSize.
##' @param onefile Only applicable if plot is a list. If plot is a list and
##'     onefile=T, all plots will be put in a pdf (file must end in pdf) with
##'     one plot per page. If plot is a list and onefile=F, numbered files will
##'     be created - one per list element.
##' @param res Resolution. Passed to png.
##' @param save Save the plot to the given file or just show? Defaults to
##'     TRUE. Hint, if you use an "exportFlag", use save=exportFlag.
##' @param show Print the plot to the screen? Defaults to the opposite of
##'     save. Hint, combining save and show in knitr can give you both a high
##'     quality plot in your pdf and a png optimized for powerpoint.
##' @param paper Only used with pdf device. See ?pdf.
##' @param debug If TRUE, browser is called to begin with.
##' @export
##' @return Nothing.
##' @examples
##' library(ggplot2)
##' writeOutput <- FALSE
##' data(pksim1,package="pmxtricks")
##' p1 <- ggplot(pksim1,aes(TIME,DV,colour=ID))+geom_point()
##' ggwrite(p1)  ## view plot on screen
##' stamp <- "note"
##' ggwrite(p1,stamp=stamp,canvas="wide",file="myplot1.png",save=writeOutput)
##' @family Plotting
##' @import grDevices
##' @import grid


### TODO
## a "show" argument is missing. Like "save" this should determine if the plot
## is printed to screen. Ideally, save and show should work independently, so
## that you can do either, both, or none, depending on these two args.
#### end TODO

ggwrite <- function(plot,file,stamp,canvas="standard",onefile=F,res=200,paper="special",save=T,show=!save,debug=F){
    
    if(debug) browser()


###### functions to be used internally
### print1 does the actual printing to the device. Because if the plot is a
### table it must be written with draw.grid, and if not by print.
    print1 <- function(plot){
        if("gtable"%in%class(plot)) {
            cat("using grid::grid.draw\n")
            grid::grid.draw(plot)
        } else {
            print(plot)
        }
    }

    ## make function to use for one plot. Then we will call tht on plot or loop
    ## it over the elements of plot in case plot is a list.
    write1 <- function(plot,fn=NULL,type,onefile=F,size){  
        
        if(!is.null(stamp)){
            plot <- ggstamp(plot,stamp)
        }
        
        if(!is.null(fn)){
            switch(type,
                   png={
                       png(filename = fn, width = 0.6 * size$width, 
                           height = 0.6 * size$height, units = "in",
                           res=res
                           ## res = 18 * max(width, height)
                           )
                   },
                   pdf={
                       pdf(file = fn, width = 0.6 * size$width, 
                           height = 0.6 * size$height,onefile=onefile,paper = paper)
                   })
            print1(plot)
            dev.off()
        }  else {
            print1(plot)
        }
    }
    


###### internal functions done

    
##### Check inputs
    
    if(missing(file)) file <- NULL
    if(!save) {
        file <- NULL
        if(onefile) onefile <- TRUE
    }
    if(missing(stamp)) stamp <- NULL
    ## If file is an empty string or null is the same.
    if(!missing(file)&&!is.null(file)){
        file2 <- gsub(" ","",file)
        if(!all(file==file2)) warning("Blank characters in filename have been removed.")
        file <- file2
    }
    
    if(!is.null(file)&&length(file)==1&&file=="") {
        file <- NULL
    }
    

    size <- canvasSize(canvas)
    ## width <- size$width
    ## height <- size$height
    
#### check inputs done

    
    writeObj <- function(plot,file,size,type){

        ## get filname extension to determine device
        type <- NULL
        fnroot <- NULL
        if(!is.null(file)){
            ## type <- sub(".+\\.(.+)$","\\1",file)
            
            type <- sub(".*\\.([^\\.]+)$","\\1",file)
            if(!type%in%c("pdf","png")) stop("Only extensions .png and .pdf are supported")
            fnroot <- sub("^(.+)\\..+$","\\1",file)
        }
        
        if(is.list(plot)&&!any(c("gg","gtable")%in%class(plot))) {
            if(onefile){
                if(type!="pdf"){
                    warning("onefile can only be used with pdf device. Will not be used.")
                    onefile <- FALSE
                }
                write1(plot,fn=file,type=type,onefile=onefile,size=size)
            } else {
                
                Nplots <- length(plot)
                ## debug
                cat("Number of plots: ",Nplots)
                Nplots.log10 <- round(log10(Nplots))
                fname.num <- function(fnroot,type,I) paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")
                if (type=="x11"){
                    write1(plot[[1]],type="x11")
                    if(Nplots>2){
                        lapply(2:Nplots,function(I){
                            ## debug
                            ## cat("opening x11 device")
                            ## x11()
                            write1(plot=plot[[I]],type=type,size=size)
                        })
                    }
                } else {
                    silent <- lapply(1:Nplots,function(I)write1(plot=plot[[I]],type=type,fn=fname.num(fnroot,type,I),size=size))
                }
            }
        } else {
            write1(plot=plot,fn=file,type=type,size=size)
        }
    }

    
    if(save){
        writeObj(plot,file=file,size=size)
    }
    if(show){
        writeObj(plot,file=NULL,size=size)
    }
    invisible()
}
