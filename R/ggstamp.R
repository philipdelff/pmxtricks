##' Always stamp your plots with script name
##'
##' This function is used to stamp ggplot type plots with datetime and
##' script name. User must provide the script name. Notice that ggplot
##' 2.2.1 or newer is required.
##'
##' @param plot The plot to be stamped.
##' @param stamp the script name. Date and time will be added
##'     automatically.
##' @param time The timestamp to be included.
##'
##' @return the plot with a stamp
##' @import ggplot2
##' @import grid
##' @importFrom gridExtra arrangeGrob
##' @importFrom utils packageVersion
##' @examples
##' library(ggplot2)
##' norms <- do.call(rbind,lapply(1:3,function(mu)data.frame(POP=mu,population=paste("Population",mu),parameter=rnorm(seq(mu-6,mu+6,.01),mean=mu,sd=sin(mu)*2))))
##' ## A very quick way to see densities is using ggplot
##' p1 <- ggplot(norms,aes(parameter,fill=population))+geom_density(alpha=.5)
##' ggwrite(p1)
##' stamp <- "note"
##' ggstamp(p1,stamp)
##' ## Or use ggwrite which will call ggstamp automatically.
##' ggwrite(p1,stamp=stamp,canvas="wide")
##' @family Plotting
##' @export


#### Todo
## 2018-10-11 ppda: date format should be changed. Output depends on locale. 
#### End todo


ggstamp <- function(plot, stamp = "no stamp",time=Sys.time()) {
### Captions are only available in ggplot 2.2.1

### Stamps
    plot.was.list <- T
    if(!( length(class(plot))==1 && class(plot)=="list" )) {
        plot.was.list <- F
        plot <- list(plot)
    }

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
        caption <- paste(date.txt,stamp)

        
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
