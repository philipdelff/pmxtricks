##' Add watermarks to ggplots
##' @param text What should be written on the plot
##' @param scale the size
##' @param rot rotation of the mark. Don't remember the unit.
##' @param col the colour (a value, not an expression) of the
##'     watermark. Default is grey.
##' @param alpha alpha value for the watermark. Default is 0.5.
##' @details This used to be based on
##'     https://www.r-bloggers.com/adding-watermarks-to-plots/ That
##'     solution stopped working, and this new solution is simpler and
##'     based on ggplot2 alone.
##' @import ggplot2
##' @family Plotting
##' @examples
##' library(ggplot2)
##' ff <- qplot(1:10, 11:20) + pmxtricks:::ggWater()


ggWater <- function(text="Not validated",scale=1,rot=30,col="grey",alpha=.5){
    
    ## watermarkGrob <- function(text = "Not validated",scale=1,rot=0){
    ##     g1 <- grob(lab=text, scale=scale,rot=rot,cl="watermark")
    ##     return(g1)
    ## }
    
    ## ## custom draw method to
    ## ## calculate expansion factor on-the-fly
    ## drawDetails.watermark <- function(x, ...){
    ##     ##    browser()
    ##     cex <- x$scale*2/3*convertUnit(unit(1,"npc"), "mm", valueOnly=TRUE) /
    ##         (convertUnit(unit(1,"grobwidth", textGrob(x$lab)), "mm",valueOnly=TRUE))

    ##     tgrob1 <- grid.text(x$lab,  rot=x$rot, gp=gpar(cex = cex, col="gray",
    ##                                                    fontface = "bold", alpha = .5))
    ##     return(tgrob1)
    ## }
    
    ## ##    annotation_custom(watermarkGrob(text,scale=scale,rot=rot))
    ## annotation_custom(grob=watermarkGrob(text,scale=scale,rot=rot),xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf)

    
    annotation_custom(textGrob(text, gp = gpar(fontsize = 80*scale,col=col,alpha=alpha),rot=rot),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)


}
