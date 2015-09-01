load("data/exact.cost.RData")
load("data/signal.list.RData")
load("data/segmentation.list.RData")
load("data/annotation.sets.RData")
load("data/L.min.max.RData")
source("scripts/ireg.signals.R")
source("scripts/geom_tallrect.R")
source("scripts/breakpoint.colors.R")
source("scripts/signal.colors.R")

## calc anns.
ann.set <- "detailed.low.density"
all.anns <- annotation.sets[[ann.set]]
all.anns$pid.chr <- with(all.anns,sprintf("%d.%s",profile.id,chromosome))
ann.df <- subset(all.anns,pid.chr%in%ireg.signals)
ann.df$annotation <- sub(">","$>$",ann.df$ann)
ann.df$annotation <- sub("normal","0breakpoints",ann.df$ann)

## calc target regions.
min.max <- L.min.max[[ann.set]][ireg.signals,]
err <- "error\n$E_i(L)$"
min.max.df <- data.frame(min.max,
                         variable=err,
                         value=0,
                         interval="target",
                         pid.chr=rownames(min.max))
getL <- function(L,latex){
  data.frame(L,latex,variable=err,pid.chr=min.max.df$pid.chr)
}
L.df <- with(min.max.df,{
  rbind(getL(min.L,"$\\underline L_i$"),
        getL(max.L,"$\\overline L_i$"))
})
vline.df <- with(min.max.df,{
  data.frame(L.hat=(min.L+max.L)/2,pid.chr)
})


opt <- "optimal number of segments\n$z_i^*(L)$"
library(reshape2)
int.df <- do.call(rbind,lapply(ireg.signals,function(pid.chr){
  intervals <- exact.cost[[ann.set]][[pid.chr]]
  ## return all intervals to display the ann cost.
  intervals[[opt]] <- intervals$segments
  intervals[[err]] <- intervals$cost
  intervals.limited <-
    intervals[,c(opt,err,"min.L","max.L")]
  molt <- melt(intervals.limited,id=c("min.L","max.L"))
  molt$interval <- "segments"
  molt$interval[molt$variable==err] <- "error"
  data.frame(molt,pid.chr)
}))
toPlot <- rbind(min.max.df,int.df)
toPlot$interval <- factor(toPlot$interval,c("target","error","segments"))

siglab <- function(var,val){
  if(var=="pid.chr"){
    sprintf("signal $i = %s$",val)
  }else as.character(val)
}

library(ggplot2)
sig.col <- signal.colors[["estimate"]]
p <- ggplot()+
  geom_vline(aes(xintercept=L.hat),colour=sig.col,data=vline.df,
             lty="dashed")+
  geom_segment(aes(min.L,value,xend=max.L,yend=value,
                   colour=interval,size=interval),data=toPlot)+
  geom_text(aes(L,label=latex),y=2,data=L.df,colour=sig.col)+
  facet_grid(variable~pid.chr,scales="free",space="free_y",labeller=siglab)+
  xlab("Linear penalty exponent $L$")+
  scale_y_continuous("",breaks=0:20,minor_breaks=NULL)+
  scale_size_manual(values=c(target=2,error=1,segments=1))+
  scale_colour_manual(values=c(target=sig.col,error="black",segments="blue"))

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-exact-kstar-cost-2.tex",h=6,w=6)

library(grid)
grid.newpage()
h <- unit(c(1,2),"null")
pushViewport(viewport(layout=grid.layout(nrow=2,heights=h)))

doPlot <- function(p,r){
  pushViewport(viewport(layout.pos.row=r))
  print(p,newpage=FALSE)
  popViewport()
}

doPlot(p,2)

sig.df <- do.call(rbind,lapply(ireg.signals,function(pid.chr){
  intervals <- exact.cost[[ann.set]][[pid.chr]]
  ## figure out optimal smoothing.
  l <- vline.df[vline.df$pid.chr==pid.chr,"L.hat"]
  k <- subset(intervals,min.L<l & l<max.L)$segments
  seg <- segmentation.list[[pid.chr]]

  data.frame(signal.list[[pid.chr]],pid.chr,smooth=seg$smooth[,k])
}))
library(plyr)
bkpts <- ddply(sig.df,.(pid.chr),function(df){
  break.after <- which(diff(df$smooth)!=0)
  do.call(rbind,lapply(break.after,function(i){
    min <- df$position[i]
    max <- df$position[i+1]
    data.frame(min,position=(min+max)/2,max)
  }))
})
seg.df <- ddply(sig.df,.(pid.chr),function(df){
  break.after <- which(diff(df$smooth)!=0)
  break.at <- sapply(break.after,function(i){
    min <- df$position[i]
    max <- df$position[i+1]
    (min+max)/2
  })
  end <- c(break.at,max(df$pos))
  start <- c(1,break.at)
  logratio <- sapply(seq_along(start),function(i){
    mean(subset(df,start[i]<position & position<end[i])$logratio)
  })
  data.frame(start,end,logratio)
})
sigplot <- ggplot()+
  geom_tallrect(aes(xmin=min/1e6,xmax=max/1e6,fill=annotation),data=ann.df)+
  geom_vline(aes(xintercept=position/1e6),data=bkpts)+
  ##geom_line(aes(position/1e6,smooth),data=sig.df,lwd=1,
  ##          colour=signal.colors["estimate"])+
  geom_point(aes(position/1e6,logratio),data=sig.df)+
  geom_segment(aes(start/1e6,logratio,xend=end/1e6,yend=logratio),
               data=seg.df,lwd=2,colour=sig.col)+
  facet_grid(.~pid.chr,labeller=siglab)+
  scale_fill_manual(values=breakpoint.colors)+
  xlab("position on chromosome (mega base pairs)")
##print(sigplot)

doPlot(sigplot,1)

dev.off()
##system("cd figures && pdflatex ireg-exact-kstar-cost-2 && evince ireg-exact-kstar-cost-2.pdf")
