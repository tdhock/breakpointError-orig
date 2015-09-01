load("data/signal.list.RData")
load("data/annotation.sets.RData")

anns <- subset(annotation.sets$simulation.manual, profile.id <= 10010)
ann.list <- split(anns,list(anns$profile.id,anns$chromosome))

signal.df <- do.call(rbind,signal.list[names(ann.list)])

library(ggplot2)
geom_tallrect <- function
### ggplot2 geom with xmin and xmax aesthetics that covers the entire
### y range.
(mapping=NULL,
 data=NULL,
 stat="identity",
 position="identity",
 ...){
  require(proto)
  require(grid)
  GeomTallRect <- proto(ggplot2:::GeomRect,{
    required_aes <- c("xmin", "xmax")
    draw <- draw_groups <- function(.,data,scales,coordinates,
                                    ymin=0,ymax=1,...){
      ymin <- unit(ymin,"npc")
      ymax <- unit(ymax,"npc")
      with(coord_transform(coordinates, data, scales),ggname(.$my_name(), {
        rectGrob(xmin, ymin, xmax - xmin, ymax-ymin,
                 default.units = "native", just = c("left", "bottom"), 
                 gp=gpar(
                   col=colour, fill=alpha(fill, alpha), 
                   lwd=size * .pt, lty=linetype, lineend="butt"
                   )
                 )
      }))
    }
  })
  GeomTallRect$new(mapping = mapping, data = data, stat = stat,
                   position = position, ...)
}
annotation.colors <- c("1breakpoint"="#ff7d7d",
                       "normal"='#f6f4bf',
                       ">0breakpoints"="#a445ee")
p <- ggplot()+
  geom_tallrect(aes(xmin=min,xmax=max,fill=annotation),
                data=anns,alpha=1)+
  geom_point(aes(position,logratio),pch=1,colour="black",data=signal.df)+
  scale_fill_manual("annotation",values=annotation.colors)+
  scale_x_continuous("position")+
  facet_grid(profile.id~chromosome,scales="free_x",space="free")+
  opts(panel.margin=unit(0,"lines"),
       title="simulated data (points) with annotations (rectangles)")

pdf("figure-ireg-check-sim.pdf")
print(p)
dev.off()
