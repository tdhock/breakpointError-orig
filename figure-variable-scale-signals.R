load("data/variable.scale.show.RData")
signal.df <- do.call(rbind,lapply(variable.scale.show,function(sig){
  with(sig,data.frame(locations,signal,mu,
                      variance.estimate=factor(round(med.abs.diff))))
}))
library(ggplot2)
source("scripts/signal.colors.R")
labeller <- function(var,val)sprintf("var. estimate\n$\\hat s_i = %s$",val)
p <- ggplot(signal.df,aes(locations,signal))+
  geom_point(pch=21)+
  geom_line(aes(y=mu),colour=signal.colors["latent"],lwd=2)+
  facet_grid(variance.estimate~.,scales="free",labeller=labeller)+
  xlab("position in base pairs")+
  ylab("simulated copy number signal")
print(p)

##pdf("figure-variable-scale-signals.pdf",h=4)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-variable-scale-signals.tex",h=3,w=6)
print(p)
dev.off()
