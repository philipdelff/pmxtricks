##' Always stamp your plots with script name
##'
##' This function is used to stamp ggplot type plots with datetime and
##' script name. User must provide the script name. 
##'
##' @param plot The plot to be stamped.
##' @param stamp the script name. Date and time will be added
##'     automatically.
##' @param file An optional output filename to be included in the stamp.
##' @param time The timestamp to be included.
##'
##' @return the plot with a stamp
##' @details The stamp is adding using the caption label. If a caption
##'     is already in the plot, the stamp will be added in a new
##'     line.
##' 
##' The caption is derived as
##' caption=paste(c(plot$label$caption,stamp,paste(date.txt,file)),collapse="\\n")
##'
##' ggplot 2.2.1 or newer is required.
##' @import ggplot2
##' @import grid
##' @importFrom gridExtra arrangeGrob
##' @importFrom utils packageVersion
##' @examples
##' library(ggplot2)
##' data(pksim1,package="pmxtricks")
##' p1 <- ggIndProfs(pksim1)[[1]]
##' stamp <- "note"
##' ggstamp(p1,stamp)
##' ## Or use ggwrite which will call ggstamp automatically.
##' ggwrite(p1,stamp=stamp,canvas="wide")
##' @family Plotting
##' @export


ggstamp <- function(plot, stamp = "no stamp", file, time=Sys.time()) {
### Captions are only available in ggplot 2.2.1
    .Deprecated("tracee::ggstamp")

### A list of plots is supported so we will run everything with lapply
    plot.was.list <- T
    if(!( length(class(plot))==1 && class(plot)=="list" )) {
        plot.was.list <- F
        plot <- list(plot)
    }
    if(missing(file)) file <- NULL
    if(!is.null(file)) file <- basename(file)

    stamp1 <- function(plot){
### determine method to use. otype is object type
        otype <- NA
        if("ggplot"%in%class(plot)){
####### for single ggplot objects
            otype <- "ggplot"
        }
        if("gtable"%in%class(plot)){
            if(!is.na(otype)) stop("Confused. type both ggplot and gtable. Dont knot how to stamp this object.")
######## for gtables as returned by arrangeGrob and grid.arrange
            otype <- "gtable"
        }
        if("ggmatrix"%in%class(plot)){
            if(!is.na(otype)) stop("Confused. type both ggmatrix and ggplot or gtable. Dont knot how to stamp this object.")
######## ggmatrix can be stamped just like ggplot
            otype <- "ggplot"
        }
        if(is.na(otype)) stop("Dont know how to stamp this object type.")
        
        date.txt <- format(time, "%d-%b-%Y %H:%M")
        caption.stamp <- paste(date.txt,file)
        caption=paste(c(plot$label$caption,stamp,caption.stamp),collapse="\n")
        
        plot.stamped <- switch(otype,
                               ggplot={if(sum(unlist(packageVersion("ggplot2")[1,])*c(1000)^c(2:0))<2002001){
                                           stop("ggplot >= 2.2.1 needed to stamp ggplot objects.")
                                       }
                                           plot+ggplot2::labs(caption=caption)+theme(plot.caption=element_text(size=6, colour="grey"))},
                               gtable={
                                   arrangeGrob(plot, bottom = textGrob(caption, gp=gpar(font=1, col = "grey", cex = 0.5)),heights=c(0.98,0.02))

                               }
                               )
        return(plot.stamped)
    }

    plot <- lapply(plot,stamp1)
    if(!plot.was.list) plot <- plot[[1]]

    return(plot)
    
}
