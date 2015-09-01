load("data/all.stats.RData")
source("scripts/fp.fn.colors.R")

cids <- c("375","362")

## This determines the plotting order
stat.names <- c("false.positive","false.negative","errors")
##disp.names <- sub(".","\n",stat.names,fixed=TRUE)
disp.names <- stat.names
ranges <- list(#flsa=10^c(-1,2),
               flsa.norm=10^c(-2,3),
               cghseg.k=10^c(-5,0),
               dnacopy.sd=10^c(-0.5,1.5)
               ##glad.haarseg=c(1e-60,1e-1),
               ##glad.lambdabreak=c(1e-1,1e5)
               )
params <- lapply(all.stats[names(ranges)],"[[","parameters")
errs <- do.call(rbind,lapply(names(params),function(a){
  df <- do.call(rbind,lapply(seq_along(stat.names),function(i){
    s <- stat.names[[i]]
    ar <- all.stats[[a]][[s]]
    m <- apply(ar,1:2,mean,na.rm=TRUE)
    ind <- do.call(rbind,lapply(cids,function(cid){
      data.frame(value=m[,cid],profile.id=paste("local model\nfor profile",cid))
    }))
    glob <- data.frame(value=apply(ar,1,mean,na.rm=TRUE),
                       profile.id="global model")
    data.frame(parameter=as.numeric(params[[a]]),
               algorithm=a,statistic=disp.names[[i]],
               rbind(glob,ind))
  }))
  curves <- subset(df,ranges[[a]][1] <= parameter & parameter <= ranges[[a]][2])
  transform(curves,percent=value*100)
}))
library(plyr)
library(bams)
picked <-
  ddply(subset(errs,statistic=="errors"),.(profile.id,algorithm),function(d){
  d[pick.best.index(d$value),]
})
plotted.min <- daply(errs,.(algorithm),with,min(parameter))
glob.err <- subset(errs,profile.id=="global model" & statistic=="errors")
min.labs <- subset(picked,profile.id=="global model")
min.labs$param.min <- plotted.min[as.character(min.labs$algorithm)]
min.labs$percent.text <- sprintf("%.1f",min.labs$percent)
alg.order <- as.character(with(min.labs,algorithm[order(percent)]))
errs$algorithm <- factor(errs$algorithm,alg.order)
picked$algorithm <- factor(picked$algorithm,alg.order)
min.labs$algorithm <- factor(min.labs$algorithm,alg.order)
library(ggplot2)
library(grid)
library(RColorBrewer)

tp <- list(vline=subset(picked,profile.id=="global model",select=-profile.id),
           min.labs=min.labs,errs=errs,picked=picked)
alg <- function(x){
  factor(gsub("cghseg.k","cghseg.k, pelt.n",x),
         c("cghseg.k, pelt.n", "flsa.norm", "dnacopy.sd"))
}
for(N in names(tp)){
  tp[[N]]$algorithm <- alg(tp[[N]]$algorithm)
}

p <- ggplot()+
  geom_vline(aes(xintercept=log10(parameter)),colour="grey",lwd=2,
    data=tp$vline)+
  geom_line(aes(log10(parameter),percent,
                colour=statistic,linetype=statistic,size=statistic),
            data=tp$errs)+
  geom_point(aes(log10(parameter),percent),data=tp$picked,size=4)+
  geom_segment(aes(x=log10(param.min),xend=log10(parameter),
                   y=percent,yend=percent),data=tp$min.labs)+
  geom_text(aes(log10(param.min),percent,label=percent.text),
            data=tp$min.labs,hjust=0,vjust=-1/2,size=4)+
  facet_grid(profile.id~algorithm,scales="free_x")+
  scale_colour_manual(values=fp.fn.colors)+
  scale_size_manual(values=fp.fn.sizes)+
  scale_linetype_manual(values=fp.fn.linetypes)+
  xlab("log10(smoothing parameter $\\lambda$)")+
  theme_bw()+ # bioinformatics
  theme(panel.margin=unit(0,"lines"))+
  ylab("percent incorrectly predicted\nannotations in training set")
print(p)

##pdf("figure-bams-learning-curves.pdf",height=6,width=8)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-bams-learning-curves.tex",height=4,width=7)
print(p)
##grid.text("log10(smoothing parameter)",0.4,0,vjust=-1)
grid.text("<- more breakpoints",0,0,hjust=0,vjust=-1)
grid.text("fewer breakpoints ->",0.9,0,hjust=1,vjust=-1)
dev.off()
