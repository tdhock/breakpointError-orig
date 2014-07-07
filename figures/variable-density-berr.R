library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
load("data/variable.density.show.RData")

lerr.df <- do.call(rbind,lapply(variable.density.show,function(sig){
  bases.per.probe <- round(max(sig$locations)/length(sig$signal))
  do.call(rbind,lapply(c(1,1/2,0),function(a){
    error <- sig$lambda.error(a)
    l.star <- sig$lambda.star(a)
    lambda <- sig$lambda
    optimal <- l.star == lambda
    data.frame(error,lambda,alpha=a,bases.per.probe,optimal)
  }))
}))
bpp <- lerr.df$bases.per.probe
lerr.df$bases.per.probe <- factor(bpp,sort(unique(bpp)))
library(ggplot2)
leg <- "bases/probe"
sizes <- c(1,0.5)
bpp.colors <- c("#00bfc4","#f8766d")
names(sizes) <- names(bpp.colors) <- levels(lerr.df$bases.per.probe)
p <- ggplot(lerr.df,aes(log10(lambda),error))+
  geom_line(aes(size=bases.per.probe,colour=bases.per.probe))+
  geom_point(aes(fill=bases.per.probe),pch=21,data=subset(lerr.df,optimal))+
  facet_grid(.~alpha,
             labeller=function(var,val)sprintf("$\\alpha = %s$",val))+
  scale_size_manual(leg,values=sizes)+
  scale_fill_manual(leg,values=bpp.colors)+
  scale_color_manual(leg,values=bpp.colors)+
  xlab("model complexity tradeoff parameter $\\log_{10}(\\lambda)$")+
  ylab("error $E_i^\\alpha(\\lambda)$")

##pdf("figures/variable-density-berr.pdf",w=7,h=2)
tikz("figures/variable-density-berr.tex",w=6,h=2)
print(p)
dev.off()


err.df <- do.call(rbind,lapply(variable.density.show,function(sig){
  bases.per.probe <- factor(round(max(sig$locations)/length(sig$signal)))
  cost <- sig$cost
  data.frame(bases.per.probe,segments=seq_along(cost),cost)
}))
library(ggplot2)
kplot <- ggplot(err.df,aes(segments,cost))+
  geom_line(aes(colour=bases.per.probe))+
  geom_point()+
  facet_grid(.~bases.per.probe,
             labeller=function(var,val)sprintf("bases/probe = %s",val))+
  scale_colour_manual(leg,values=bpp.colors,guide="none")+
  scale_x_continuous("Number of segments of estimated cghseg model",
                     breaks=c(1,7,20),minor_breaks=NULL)+
  scale_y_continuous("Error relative to\nlatent breakpoints",
                     breaks=c(0,1,6,13,14),minor_breaks=NULL)

##pdf("figures/variable-density-berr-k.pdf",h=3)
tikz("figures/variable-density-berr-k.tex",h=2.5,w=6)
print(kplot)
dev.off()
