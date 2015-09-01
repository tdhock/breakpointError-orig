load("data/model.comparison.RData")
library(plyr)
model.stats <- ddply(model.comparison$error,.(model,ann.set),function(d){
  d$test.error.percent <- with(d,test.errors/test.anns*100)
  result <- data.frame()
  for(stat in c("test.error.percent","seconds")){
    x <- d[,stat]
    if(stat == "seconds"){
      x <- log10(x)
      stat <- "log10(seconds)"
    }
    result <- rbind(result,{
      data.frame(mean=mean(x),sd=sd(x),stat)
    })
  }
  result
})
## display best model first.
error.df <- subset(model.stats,
                   stat=="test.error.percent" &
                   ann.set=="detailed.low.density")
levs <- as.character(with(error.df,model[order(mean)]))
model.stats$model <- factor(model.stats$model,levs)
library(ggplot2)
### geoms useful for plotting these data using ggplot2
geom_hrange <- function
(mapping=NULL,
 data=NULL,
 stat="identity",
 position="identity",
 ...){
  require(ggplot2)
  require(proto)
  GeomHorizontalRange <- proto(ggplot2:::GeomLinerange,{
    required_aes <- c("xmin","y","xmax")
    draw <- function(., data, scales, coordinates, ...) {
      munched <- ggplot2:::coord_transform(coordinates, data, scales)
      ggname(.$my_name(),
             GeomSegment$draw(transform(data, yend=y, x=xmin, xend=xmax),
                              scales, coordinates, ...))
    }
  })
  GeomHorizontalRange$new(mapping = mapping, data = data, stat = stat,
                          position = position, ...)
}
toplot <- subset(model.stats,stat=="test.error.percent")
p <- ggplot(toplot,aes(mean,model))+
  geom_point()+
  geom_hrange(aes(xmin=mean,xmax=mean+sd))+
  facet_grid(.~ann.set,scales="free")+
  xlab("mean +1 standard deviation")+
  ggtitle(paste("Breakpoint detection error estimated using",
          "10-fold cross-validation"))
pdf("figure-ireg-compare-model-error.pdf",w=10,h=2)
print(p)
dev.off()#;system("evince figure-ireg-compare-model-error.pdf")
