theme_pp <- function(...){
this.theme <- theme_classic()+
    theme(
##        theme0=theme_classic,
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position="bottom")+
#        theme(plot.title=element_text(size=18,face="bold",hjust=.5,margin=unit(c(
#                                                                       3,0,3,0),"mm")))+
        theme(legend.title=element_blank())+
        ## increment font size in legend
        theme(legend.text=element_text(size=16,colour="black"))+
### axis
         theme(axis.title= element_text(size = 16,colour="black"))+
         ## theme(
         ##     axis.title.x = element_text(margin = unit(c(8, 0, 8, 0), "mm")),
         ##     axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm")))+
## ### text size in facet labels
    theme(strip.text = element_text(size = 16,colour="black"))+
## ### background colour for the facet labels
         theme(strip.background=element_rect(fill="white",colour="white" ))+
## ### black frame, black tick labels?
##    theme(panel.border = element_blank(colour = "white"))+
## ### remove grid lines
         theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())+
    theme(panel.spacing = unit(4, "lines"))

## if(!missing(...)) this.theme <- this.theme()+theme(...)
### taking a chance here - not sure. If it does, we can just add it
### without using this.theme as name at all.
this.theme <- this.theme()+theme(...)

this.theme
}
