set.seed(1)
loci <- data.frame(score=c(rbeta(800,10,10),
                     rbeta(100,0.15,1),
                     rbeta(100,1,0.25)),
  type=factor(c(rep("Neutral",800),
    rep("Positive",100),
    rep("Balancing",100)))) 
head(loci)
library(lattice)
dens <- densityplot(~score,loci,groups=type,ylim=c(-0.5,11.5),
  auto.key=list(space="top",columns=3),n=500)

library(directlabels)

##pdf("figure-dens-confusing.pdf",h=5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-dens-confusing.tex",h=4.3,w=6)
dlcompare(list(dens),
          list(Legend="legend","Direct labels"="top.points"),
          rects=FALSE,
          row.items="posfuns")
dev.off()
