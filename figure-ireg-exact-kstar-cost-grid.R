load("data/exact.cost.RData")
load("data/segmentation.list.RData")
load("data/cost.matrices.RData")
source("scripts/ireg.signals.R")
library(reshape2)
ann.set <- "detailed.low.density"
cost.mats <- cost.matrices[[ann.set]]
int.df <- do.call(rbind,lapply(ireg.signals[1],function(pid.chr){
  intervals <- exact.cost[[ann.set]][[pid.chr]]
  intervals[["optimal number of segments"]] <- intervals$segments
  intervals.limited <-
    intervals[,c("optimal number of segments","cost","min.L","max.L")]
  molt <- melt(intervals.limited,id=c("min.L","max.L"))
  data.frame(molt,pid.chr)
}))
grid.df <- do.call(rbind,lapply(ireg.signals[1],function(pid.chr){
  df <- int.df[int.df$pid.chr==pid.chr,]
  L.grid <- with(df,seq(min(max.L)-1,max(min.L)+1,l=100))
  seg.info <- segmentation.list[[pid.chr]]
  J <- seg.info$J
  K <- seq_along(J)
  kstar.grid <- sapply(L.grid,function(L){
    which.min(exp(L)*K+J)
  })
  cost.mat <- cost.mats[[pid.chr]]
  cost.k <- colSums(cost.mat)
  cost.grid <- cost.k[kstar.grid]
  d <- rbind(data.frame(L=L.grid,value=kstar.grid,
                   variable="optimal number of segments"),
             data.frame(L=L.grid,value=cost.grid,variable="cost"))
  data.frame(d,pid.chr)
}))
library(ggplot2)
p <- ggplot()+
  geom_segment(aes(min.L,value,xend=max.L,yend=value),data=int.df,lwd=0.8)+
  geom_point(aes(L,value),data=grid.df,colour="red",pch=1)+
  facet_grid(variable~.,scales="free",space="free")+
  xlab("Linear penalty exponent $L$")+
  scale_y_continuous("",breaks=0:20,minor_breaks=NULL)
##pdf("figure-ireg-exact-kstar-cost-grid.pdf",h=3.5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-exact-kstar-cost-grid.tex",h=3.5,w=6)
print(p)
dev.off()
