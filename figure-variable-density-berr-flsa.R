load("data/variable.density.show.RData")

lerr.df <- do.call(rbind,lapply(variable.density.show,function(sig){
  bases.per.probe <- round(max(sig$locations)/length(sig$signal))
  do.call(rbind,lapply(c(2,1,0),function(a){
    lambda <- sig$flsa.lambda
    error <- sig$flsa.cost[sig$flsa.alpha==a,]
    is.min <- which(min(error)==error)
    l.star <- lambda[floor(mean(is.min))]
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
  scale_x_continuous("model complexity tradeoff parameter $\\log_{10}(\\lambda)$",
                     breaks=c(-5,0,5))+
  scale_y_continuous("error $E_i^\\alpha(\\lambda)$")

##pdf("figure-variable-density-berr-flsa.pdf",w=7,h=2)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-variable-density-berr-flsa.tex",w=7,h=2)
print(p)
dev.off()


