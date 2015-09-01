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
toplot <- subset(model.stats,
                 !grepl("simulation",ann.set) &
                 model%in%c("cghseg.k","log.n","log.hall.log.n","lasso"))
FINDREP <- c(log.n="log.d",log.hall.log.n="log.s.log.d",lasso="L1-reg")
torep <- levels(toplot$model)%in%names(FINDREP)
levels(toplot$model)[torep] <- FINDREP[levels(toplot$model)[torep]]
levs <- c("L1-reg","log.s.log.d","log.d","cghseg.k")
toplot$model <- factor(toplot$model,levs)
Time <- function(label,seconds){
  data.frame(label,mean=log10(seconds),
             stat="log10(seconds)",model="cghseg.k")
}
times <- rbind(#Time("1 millisecond",1/1000),
               Time("1 sec",1),
               Time("1 min",60))
library(grid)
levels(toplot$ann.set) <- gsub("[.]","\n",levels(toplot$ann.set))
p <- ggplot(toplot,aes(mean,model))+
  geom_point()+
  geom_text(aes(label=label),data=transform(times,ann.set="original"),
            hjust=0,size=4)+
  geom_vline(aes(xintercept=mean),data=times,colour="grey")+
  geom_hrange(aes(xmin=mean,xmax=mean+sd))+
  facet_grid(ann.set~stat,scales="free")+
  scale_x_continuous("mean +1 standard deviation",minor_breaks=NULL)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  ylab("model\n")

##pdf("figure-ireg-compare-models.pdf",h=5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-ireg-compare-models.tex",h=3,w=6)
print(p)
dev.off()
