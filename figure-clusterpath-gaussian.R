load("data/gaussian.RData")

d$kmeans <- factor(d$kmeans)
library(ggplot2)
library(directlabels)
p <- ggplot(,aes(x,y))+
  geom_path(aes(group=row),data=res,col="grey")+
  geom_point(aes(colour=kmeans),data=d)+
  geom_dl(aes(label=kmeans,colour=kmeans),data=d,method="smart.grid")+
  guides(colour="none")+
  theme_bw()
##pdf("figure-clusterpath-gaussian.pdf")
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
png("figure-clusterpath-gaussian.png",h=6,w=6,units="in",res=600)
print(p)
dev.off()
