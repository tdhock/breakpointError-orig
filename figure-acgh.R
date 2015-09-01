data(neuroblastoma,package="neuroblastoma")
nb.list <- split(neuroblastoma$pr,neuroblastoma$pr$pr)
pro <- subset(nb.list[["18"]],chromosome%in%c(1:8))
levels(pro$chromosome) <- sprintf("chr%s",levels(pro$chromosome))
library(ggplot2)
library(grid)
p <- ggplot(,aes(position/1e6,logratio))+
  geom_point(data=pro)+
  facet_grid(.~chromosome,scales="free",space="free")+
  theme_bw()+
  theme(panel.margin=unit(0,"in"))+
  scale_x_continuous("position on chromosome (mega base pairs)",
                     breaks=c(100,200))

##pdf("figure-acgh.pdf",h=2)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-acgh.tex",h=2)
print(p)
dev.off()
