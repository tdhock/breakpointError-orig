data(BodyWeight,package="nlme")
library(ggplot2)
BodyWeight$ID <- BodyWeight$Rat
ratplot <- ggplot(BodyWeight,aes(Time,weight,colour=ID))+
  facet_grid(.~Diet)+
  geom_line()+
  xlim(0,70)+
  guides(col=guide_legend(ncol=2))
library(directlabels)
methods <- list(Legend="legend","Direct labels"="last.qp")

##pdf("figure-rat-unreadable.pdf",h=6)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-rat-unreadable.tex",h=4.5,w=6)
dlcompare(list(ratplot),methods,rects=FALSE,row.items="posfuns")
dev.off()
