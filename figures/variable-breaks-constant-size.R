load("data/variable.breaks.constant.size.show.RData")
signal.df <-
  do.call(rbind,lapply(variable.breaks.constant.size.show,function(sig){
    with(sig,data.frame(locations,signal,mu,length=max(locations)))
  }))
library(ggplot2)
source("scripts/signal.colors.R")
p <- ggplot(signal.df,aes(locations,signal))+
  geom_point(pch=21)+
  geom_line(aes(y=mu),colour=signal.colors["latent"],lwd=1.1)+
  facet_grid(length~.,
             labeller=function(var,val)sprintf("length = %s",val))+
  xlab("position in base pairs")
print(p)

pdf("figures/variable-breaks-constant-size.pdf",h=4.2)
##library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
##tikz("figures/variable-breaks-constant-size.tex",h=4.2,w=6)
print(p)
dev.off()
