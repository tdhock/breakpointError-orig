load("data/exact.cost.RData")
ann.set <- "detailed.low.density"
source("scripts/ireg.signals.R")
library(reshape2)
int.df <- do.call(rbind,lapply(ireg.signals[1],function(pid.chr){
  intervals <- exact.cost[[ann.set]][[pid.chr]]
  intervals[["optimal number of segments"]] <- intervals$segments
  intervals.limited <-
    intervals[,c("optimal number of segments","cost","min.L","max.L")]
  molt <- melt(intervals.limited,id=c("min.L","max.L"))
  data.frame(molt,pid.chr)
}))
library(ggplot2)
p <- ggplot()+
  geom_segment(aes(min.L,value,xend=max.L,yend=value),data=int.df,lwd=0.8)+
  facet_grid(variable~.,scales="free",space="free")+
  xlab("log(gamma) degree of smoothness")+
  scale_y_continuous("",breaks=0:20,minor_breaks=NULL)

pdf("figure-ireg-exact-kstar-cost.pdf",h=3.5)
print(p)
dev.off()
