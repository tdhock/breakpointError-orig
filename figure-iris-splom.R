library(clusterpath)
irisSc <- scale(iris[,-5])
library(RColorBrewer)
custom.pal <- brewer.pal(3,"Set2")
trellis.par.set(theme=simpleTheme(col=custom.pal,pch=1:3))
path <- clusterpath.l2(irisSc,gamma=1,join.thresh=0.05,
                       opt.thresh=0.1,
                       verbose=1,lambda.factor=1.05)
##pdf("figure-iris-splom.pdf",h=7.5,w=10)
##library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
##tikz("figure-iris-splom.tex")
png("figure-iris-splom.png",h=1100,w=1300,res=150)
trellis.par.set(theme=simpleTheme(col=custom.pal,pch=1:3))
plot(path,groups=iris$Species,auto.key=list(space="top",columns=3))
dev.off()

