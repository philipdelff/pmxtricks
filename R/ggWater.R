##' get watermarks in ggplots
##' @param text What should be written on the plot
##' @param scale the size
##' @param rot rotation of the mark. Don't remember the unit.
##' @details taken from https://www.r-bloggers.com/adding-watermarks-to-plots/

##' @examples ff <- qplot(1:10, 11:20) + qcp_water()
##' @export
ggWater <- function(text="Not validated",scale=1,rot=0){
    
    watermarkGrob <- function(text = "Not validated",scale=1,rot=0){
        g1 <- grob(lab=text, scale=scale,rot=rot,cl="watermark")
        return(g1)
    }
    
    ## custom draw method to
    ## calculate expansion factor on-the-fly
    drawDetails.watermark <- function(x, ...){
        library(grid)
        ##    browser()
        cex <- x$scale*2/3*convertUnit(unit(1,"npc"), "mm", val=TRUE) /
        (convertUnit(unit(1,"grobwidth", textGrob(x$lab)), "mm",val=TRUE))

    tgrob1 <- grid.text(x$lab,  rot=x$rot, gp=gpar(cex = cex, col="gray",
                                                   fontface = "bold", alpha = .5))
    return(tgrob1)
}
    

    annotation_custom(watermarkGrob(text,scale=scale,rot=rot))
}
