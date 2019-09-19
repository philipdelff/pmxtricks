##' Extract the legend from a ggplot
##' @description You often want this to share a legend between multiple plots that do not also share axes (i.e. you can't use faceting).
##' @param plot A plot made with ggplot.
##' @details There is (probably much better) functionality in the package ggpubr to use common legend for multiple plots. However, ggpubr depends on latest R version so it can give trouble.
##' The code was taken from https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots which has a ref to
##' https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
##' @examples 
##' This example is not done yet. But it shows the idea
##' mylegend <- extractLegend(p1)
##' p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
##'                                p2 + theme(legend.position="none"),
##'                                nrow=1),
##'                                mylegend, nrow=2,heights=c(10, 1))
##' @family plotting
##' @importFrom ggplot2 ggplot_gtable ggplot_build
##' @export

extractLegend <- function(p){
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  ## if no legend in plot
  if(length(leg)==0) return(NULL)
  legend <- tmp$grobs[[leg]]
  return(legend)
}
