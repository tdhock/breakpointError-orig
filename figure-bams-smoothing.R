data(neuroblastoma,package="neuroblastoma")
interesting <- "375"
profile <- subset(neuroblastoma$profiles,profile.id==interesting)
ann <- subset(neuroblastoma$annotations,profile.id==interesting)
library(bams)
library(flsa)
library(plyr)
unique.positions <-
  ddply(profile,.(chromosome,position),summarize,logratio=mean(logratio))
lvals <- c(0.5,7.5,10)
smooth <- ddply(unique.positions,.(chromosome),function(d){
  model <- flsa(d$logratio)
  sol <- flsaGetSolution(model,lambda2=lvals)
  do.call(rbind,lapply(seq_along(lvals),function(i){
    data.frame(d,smooth=sol[i,],lambda=lvals[i])
  }))
})
bkpts <- ddply(smooth,.(chromosome,lambda),function(d){
  subset(with(d,{
    data.frame(position=position[-length(position)]+diff(position)/2,
               breakpoint=diff(d$smooth))
  }),breakpoint!=0)
})
library(ggplot2)
library(grid)
llab <- function(var,val){
  if(var=="lambda")sprintf("$\\lambda = %s$",val)
  else as.character(val)
}

source("scripts/signal.colors.R")
smooth.col <- signal.colors["estimate"]
source("scripts/breakpoint.colors.R")
rect.width <- 8

chroms <- as.character(c(1,2,3,4,7,17))
bkpts <- subset(bkpts,chromosome%in%chroms)
smooth <- subset(smooth,chromosome%in%chroms)

p <- ggplot()+
  geom_tallrect(aes(xmin=min/1e6,xmax=max/1e6,fill=annotation),
                data=ann,alpha=1)+
##   geom_tallrect(aes(xmin=position/1e6-rect.width/2,
##                     xmax=position/1e6+rect.width/2),
##                 colour="black",
##                 fill=smooth.col,data=bkpts)+
  geom_vline(aes(xintercept=position/1e6),data=bkpts)+
  geom_point(aes(position/1e6,logratio),pch=1,colour="black",data=smooth)+
  scale_fill_manual("annotation",values=breakpoint.colors)+
  scale_x_continuous("position on chromosome $p$ (mega base pairs)",
                     breaks=c(100,200))+
  scale_y_continuous("logratio $y$",breaks=c(-1,0,1),limits=c(-1.2,1.2))+
  facet_grid(lambda~chromosome,scales="free_x",space="free",labeller=llab)+
  geom_line(aes(position/1e6,smooth),colour=smooth.col,size=1.5,data=smooth)+
  theme_bw()+ # bioinformatics
  theme(panel.margin=unit(0,"lines"))

#pdf("figure-bams-smoothing.pdf",height=3.6,width=6)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-bams-smoothing.tex",w=6,h=3)
print(p)
dev.off()
