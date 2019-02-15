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
##' @param canvas Can either be a character with one of the values "standard",
##'     "wide", "A4", "xeqy","wide-screen" or it can be a list with elements
##'     width and height with single values (unit is inches).
##' @param onefile Only applicable if plot is a list. If plot is a list and
##'     onefile=T, all plots will be put in a pdf (file must end in pdf) with
##'     one plot per page. If plot is a list and onefile=F, numbered files will
##'     be created - one per list element.
##' @param res Resolution. Passed to png.
##' @param save Save the plot to the given file or just show? Defaults to
##'     TRUE. Hint, if you use an "exportFlag", use save=exportFlag.
##' @param debug If TRUE, browser is called to begin with.
##' @export
##' @return Nothing.
##' @examples
##' exportFlag <- T
##' norms <- do.call(rbind,lapply(1:3,function(mu)data.frame(POP=mu,population=paste("Population",mu),parameter=rnorm(seq(mu-6,mu+6,.01),mean=mu,sd=sin(mu)*2))))
##' ## A very quick way to see densities is using ggplot
##' p1 <- ggplot(norms,aes(parameter,fill=population))+geom_density(alpha=.5)
##' ggwrite(p1)  ## view plot on screen
##' stamp <- "note"
##' ggwrite(p1,stamp=stamp,canvas="wide",save=exportFlag)

### TODO
## a "show" arguement is missing. Like "save" this should determine if the plot
## is printed to screen. Ideally, save and show should work independently, so
## that you can do either, both, or none, depending on these two args.
#### end TODO

ggwrite <- function(plot,file,stamp,canvas="standard",onefile=F,res=200,debug=F,save=T){
    require(grid)
    ## labeling?
    ## gridExtra?
    
    if(debug) browser()


###### functions to be used internally
### print1 does the actual printing to the device. Because if the plot is a table it must be written with draw.grid
    print1 <- function(plot){
        if("gtable"%in%class(plot)) {
            cat("using grid.draw\n")
            grid.draw(plot)
        } else {
            print(plot)
        }
    }

   
    ## make function to use for one plot. Then we will call tht on plot or loop
    ## it over the elements of plot in case plot is a list.
    write1 <- function(plot,fn=NULL,type,onefile=F){  
        
        if(!is.null(stamp)){
            plot <- ggstamp(plot,stamp)
        }
        
        if(!is.null(fn)){
            if(type=="png"){
                png(filename = fn, width = 0.6 * width, 
                    height = 0.6 * height, units = "in",
                    res=res
                    ## res = 18 * max(width, height)
                    )
                ## png(filename = fn, width = width, 
                ##     height =  height, units = "in",
                ##     res=200
                ##     ## res = 18 * max(width, height)
                ##     )
            }
            if(type=="pdf"){
                pdf(file = fn, width = 0.6 * width, 
                    height = 0.6 * height,onefile=onefile)
            }
            print1(plot)
            dev.off()
        }  else {
            print1(plot)
        }
    }


### a function that looks up the canvas size
    canvasSize <- function(canvas){
        
        ## size of plot
        ## A "screen" version is needed that will save graohics nice to read on screen. Could be like 1.4*standard.
        if(is.list(canvas) ){ if (all(c(!is.null(canvas$height),!is.null(canvas$width)))) {
                                  ## todo: width and height must be numerics of length one 
                                  height <- canvas$height
                                  width <- canvas$width
                              } else {
                                  stop("Canvas is a list but does not include height and width")
                              }
        } else {
            ## todo must be a character of length one
            possible.canvases <- list(
                standard=list(width=12,height=9),
                wide=list(width=16,height=9),
                A4=list(width=9,height=12),
                xeqy=list(width=9,height=9),
                "wide-screen"=list(width=31,height=15)
            )
            ## browser()                
            size.matched <- grep(paste0("^ *",canvas," *$"),names(possible.canvases),ignore.case=T)
            if(length(size.matched)!=1) stop(
                                            paste("canvas has to match exactly one of",paste(names(possible.canvases),collapse=", "),". Matching is not case-sensitive.")
                                        )
            width <- possible.canvases[[size.matched]]$width
            height <- possible.canvases[[size.matched]]$height
            ## c(12, 16, 9, 9,31)[]
            ## width <- c(12, 16, 9, 9,31)[c("standard", "wide", "A4", "xeqy","wide-screen") == canvas]
            ## height <- c(9, 9, 12, 9,15)[c("standard","wide", "A4", "xeqy","wide-screen") == canvas]
        }
        
        return(list(width=width,height=height))
    }
    
###### internal functions done

    
##### Check inputs
    if(missing(file)) file <- NULL
    if(!save) {
        file <- NULL
        if(onefile) onefile <- T
    }
    if(missing(stamp)) stamp <- NULL
    ## If file is an empty string or null is the same.
    if(!missing(file)&&!is.null(file)){
        file2 <- gsub(" ","",file)
        if(file!=file2) warning("Blank characters in filename have been removed.")
        file <- file2
    }
    
    if(!is.null(file)&&file=="") {
        file <- NULL
    }
    
    ## get filname extension to determine device
    type <- "x11"
    fnroot <- NULL
    if(!is.null(file)){
        type <- sub(".+\\.(.+)$","\\1",file)
        if(!type%in%c("pdf","png")) stop("Only extensions .png and .pdf are supported")
        fnroot <- sub("^(.+)\\..+$","\\1",file)
    }

    size <- canvasSize(canvas)
    width <- size$width
    height <- size$height
    
#### check inputs done
    
    if(is.list(plot)&&!any(c("gg","gtable")%in%class(plot))) {
        if(onefile){
            if(type!="pdf"){stop("onefile can only be used with pdf device.")}
            write1(plot,fn=file,type=type,onefile=T)
        } else {
            
            Nplots <- length(plot)
            Nplots.log10 <- round(log10(Nplots))
            fname.num <- function(fnroot,type,I) paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")
            if (type=="x11"){
                write1(plot[[1]],type="x11")
                if(Nplots>2){
                    lapply(2:Nplots,function(I){
                        x11()
                        write1(plot=plot[[I]],type=type)
                    })
                }
            } else {
                silent <- lapply(1:Nplots,function(I)write1(plot=plot[[I]],type=type,fn=fname.num(fnroot,type,I)))
            }
            
            
        }
    } else {
        write1(plot=plot,fn=file,type=type)
    }
    
    invisible()
}
