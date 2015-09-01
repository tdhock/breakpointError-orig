load("data/all.stats.RData")
library(bams)
roc <- data.frame()
source("scripts/algo.colors.R")
for(algorithm in intersect(names(algo.colors),names(all.stats))){
  stat <- all.stats[[algorithm]]
  normal.anns <- sum(stat$normal.anns)
  breakpoint.anns <- sum(stat$breakpoint.anns)
  correct <- stat$errors==0
  TPR <- apply(correct,1,function(correct.mat){
    sum(correct.mat & stat$breakpoint.anns) / breakpoint.anns
  })
  FPR <- apply(stat$errors,1,function(error.mat){
    sum(error.mat & stat$normal.anns) / normal.anns
  })
  errors <- apply(stat$errors,1,sum,na.rm=TRUE)
  class <- gsub("[.].*","",algorithm)
  class <- switch(class,
                  cghFLasso="optimization",
                  flsa="optimization",
                  cghseg="optimization",
                  pelt="optimization",
                  gada="dnacopy",
                  class)
  class <- gsub("optimization","optimization-based models",class)
  class <- gsub("dnacopy","approximate optimization",class)
  best.i <- pick.best.index(errors)
  best <- rep(FALSE,length(errors))
  best[best.i] <- TRUE
  newroc <- data.frame(class,algorithm,parameter=stat$parameters,
                       TPR,FPR,errors,best)
  roc <- rbind(roc,newroc)
}
library(lattice)
##xyplot(TPR~FPR|algorithm,roc,type="o")
no.tuning <- names(all.stats)[sapply(all.stats,function(L)dim(L$errors)[1])==1]
curves <- subset(roc,!algorithm%in%no.tuning)
## Need to reorder factor to get different looking colors in the same
## panel
algo.class <- unique(curves[,c("algorithm","class")])
algo.class <- algo.class[order(algo.class$class),]
lev.list <- lapply(levels(algo.class$class),function(lev){
  as.character(subset(algo.class,class==lev)$algorithm)
})
levs <- c()
for(i in 1:max(sapply(lev.list,length))){
  for(v in lev.list){
    if(length(v)>=i)levs <- c(levs,v[i])
  }
}
curves$algorithm <- factor(curves$algorithm,levs)
dots <- subset(roc,algorithm%in%no.tuning)
dots$vjust <- 2
dots$hjust <- -0.1
dots$label <- as.character(dots$algorithm)
colordots <- subset(curves,best)
colordots$hjust <- -0.1
colordots$vjust <- 1.1
colordots$label <- as.character(colordots$algorithm)
change <- list(hjust=c(dnacopy.alpha=1.05,glad.haarseg=0.5,flsa=-0.5,
                 glad.MinBkpWeight=0.5,dnacopy.default=0.9,
                 pelt.default=-0.5,
                 cghseg.k=-1.1,pelt.n=-2,
                 gada=-0.2,
                 flsa.norm=-0.7,
                 cghseg.mBIC=1),
               vjust=c(dnacopy.alpha=0.6,glad.haarseg=-2.5,
                 gada=-0.5,flsa.norm=0.5,
                 glad.default=1.2,
                 dnacopy.prune=1.5,
                 cghseg.k=1.1,pelt.n=-0.1,
                 glad.MinBkpWeight=2,dnacopy.default=2),
               label=c(
                 dnacopy.default=" dnacopy\ndefault",
                 glad.default="glad\ndefault",
                 glad.MinBkpWeight="glad\nMinBkpWeight",
                 dnacopy.alpha="dnacopy\nalpha",
                 dnacopy.prune="dnacopy\nprune"))
for(coln in names(change)){
  v <- change[[coln]]
  for(N in names(v)){
    colordots[colordots$algorithm==N,coln] <- v[N]
    dots[dots$algorithm==N,coln] <- v[N]
  }
}

library(ggplot2)
library(grid)
dotsize <- 4
text.cex <- 4
curves <- subset(curves,algorithm!="pelt.n")
p <- ggplot(roc,aes(FPR,TPR))+
  facet_grid(.~class)+
  geom_path(aes(colour=algorithm),data=curves,lwd=1.5)+
  geom_path(aes(group=algorithm),data=curves,lty="dashed")+
  geom_point(data=dots,fill="black",colour="black",pch=21,size=dotsize)+
  geom_point(fill=NA,pch=21,data=colordots,size=dotsize)+
  geom_text(aes(colour=algorithm,label=label,hjust=hjust,vjust=vjust),
            data=colordots,cex=text.cex)+
  geom_text(aes(label=label,hjust=hjust,vjust=vjust),data=dots,
            cex=text.cex)+
  coord_cartesian(xlim=c(0,0.5),ylim=c(0.5,1))+
  scale_x_continuous("False positive rate = probability(predict breakpoint | normal)",
                     breaks=seq(0,0.4,by=0.1))+
  scale_y_continuous(paste("True positive rate =\n",
                           "prob(predict breakpoint | breakpoint)",
                           sep=""),
                     breaks=seq(0.5,1,by=0.1))+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))+
  scale_colour_manual(values=algo.colors,guide="none")

##pdf("figure-bams-roc.pdf",width=8.3,height=3.4)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-bams-roc.tex",width=7.3,height=3.4)

## for publishing on lulu.com we need to make a pdf with all fonts
## embedded.

## R does not embed fonts by default.

## Two possible solutions.

## 1. convert all figures to tikz, and compile with pdflatex, which
## makes pdfs with embedded fonts.

## 2. use the embedFonts R function to make a pdf with embedded fonts.

## embedFonts("figure-bams-roc.pdf",outfile="figures/bams-roc-embed.pdf",fontpaths="/usr/share/texmf-texlive/fonts/afm/adobe/zapfding")

## then use pdffonts bams-roc-embed.pdf to see if all fonts have been
## embedded.

## however I was getting the following output which means that
## nimbusSans is embedded but Zapf Dingbats is not.

## hocking@fl-58017:~/bioviz/HOCKING-phd-thesis/figures$ pdffonts bams-roc-embed.pdf 
## name                                 type              emb sub uni object ID
## ------------------------------------ ----------------- --- --- --- ---------
## ZapfDingbats                         Type 1            no  no  no      12  0
## PMBECZ+NimbusSanL-Regu               Type 1C           yes yes no      10  0



##par(family="NimbusSan")
print(p)
dev.off()
