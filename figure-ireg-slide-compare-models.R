works_with_R("2.15.1",grid="2.15.1",ggplot2="0.9.2.1",plyr="1.7.1")
load("data/model.comparison.RData")
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
levs <- c("lasso","log.hall.log.n","cghseg.k")
model.stats$model <- factor(model.stats$model,levs)
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
toplot <- subset(model.stats,stat=="test.error.percent" &
                 model%in%c("cghseg.k","log.n","log.hall.log.n","lasso") &
                 ann.set %in% c("original","detailed.low.density"))
FINDREP <- c(log.n="log.d",log.hall.log.n="log.s.log.d",lasso="L1-reg")
torep <- levels(toplot$model)%in%names(FINDREP)
levels(toplot$model)[torep] <- FINDREP[levels(toplot$model)[torep]]
p <- ggplot(toplot,aes(mean,model))+
  geom_point()+
  geom_hrange(aes(xmin=mean,xmax=mean+sd))+
  facet_grid(.~ann.set,scales="free")+
  xlab("percent test annotation error, mean +1 standard deviation")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))

pdf("figure-ireg-slide-compare-models.pdf",h=1.7,w=8)
print(p)
dev.off()
