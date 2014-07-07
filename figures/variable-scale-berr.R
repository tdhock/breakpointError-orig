load("data/variable.scale.show.RData")

lerr.df <- do.call(rbind,lapply(variable.scale.show,function(sig){
  do.call(rbind,lapply(c(3,2,1,0),function(a){
    error <- sig$lambda.error(a)
    l.star <- sig$lambda.star(a)
    lambda <- sig$lambda
    optimal <- l.star == lambda
    data.frame(error,lambda,alpha=a,optimal,
               variance.estimate=sig$med.abs.diff)
  }))
}))
s.hat <- round(lerr.df$variance.estimate)
lerr.df$variance.estimate <- factor(s.hat,sort(unique(s.hat)))
library(ggplot2)
leg <- "variance\nestimate"
sizes <- c(1.5,0.5)
s.colors <- c("#00bfc4","#f8766d")
names(sizes) <- names(s.colors) <- levels(lerr.df$variance.estimate)
p <- ggplot(lerr.df,aes(log10(lambda),error))+
  geom_line(aes(size=variance.estimate,colour=variance.estimate))+
  geom_point(aes(fill=variance.estimate),pch=21,data=subset(lerr.df,optimal))+
  facet_grid(.~alpha,
             labeller=function(var,val)sprintf("$\\alpha = %s$",val))+
  xlab("model complexity tradeoff parameter $\\log_{10}(\\lambda)$")+
  ylab("error $E_i^\\alpha(\\lambda)$")+
  scale_size_manual(leg,values=sizes)+
  scale_fill_manual(leg,values=s.colors)+
  scale_color_manual(leg,values=s.colors)

##pdf("figures/variable-scale-berr.pdf",w=7,h=3)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figures/variable-scale-berr.tex",w=6,h=3)
print(p)
dev.off()


