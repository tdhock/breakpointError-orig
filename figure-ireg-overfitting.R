## Plot the learning curves to see if we can overfit with the
## multidimensional interval regression model.

load("data/min.test.df.RData")
load("data/overfit.df.RData")

library(ggplot2)
p <- ggplot(overfit.df,aes(-log10(gamma),value,colour=set))+
  geom_line(lwd=1.5)+
  geom_vline(aes(xintercept=-log10(gamma)),data=min.test.df)+
  facet_grid(stat~.,scales="free")+
  ylab("")+
  xlab("-log10(gamma) model complexity")
library(directlabels)
dl <- direct.label(p,list("last.points",hjust=1,vjust=-0.5))
print(dl)

##pdf("figure-ireg-overfitting.pdf",h=4.5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-overfitting.tex",h=4.5)
print(dl)
dev.off()
